module Blarney.MMReg where

-- Blarney imports
import Blarney
import Blarney.Stmt
import Blarney.Queue
import Blarney.Vector qualified as V
import Blarney.SourceSink

-- AXI4 imports
import Blarney.AXI4
import Blarney.AXI4.Utils.BufferShim

-- A MMReg is a memory-mapped register of a desired width
makeMMReg :: forall params. _ => Module (AXI4_Subordinate params)
makeMMReg = do
  -- Declare a shim to implement the interface in a simple manner
  shim <- makeAXI4BufferShim (makePipelineQueue 1)

  -- Data registers
  dataRegs :: V.Vec (2 ^ AddrWidth params) (Reg (Bit 8)) <-
    V.replicateM (makeReg dontCare)

  -- Read-related registers
  arid    <- makeReg dontCare
  araddr  <- makeReg dontCare
  arsize  <- makeReg dontCare
  arcount <- makeReg dontCare

  -- Write-related registers
  awid   <- makeReg dontCare
  awaddr <- makeReg dontCare
  awsize <- makeReg dontCare

  -- Read/write in progress?
  read  <- makeReg false
  write <- makeReg false

  always do
    -- Receive write request
    when (inv write.val .&&. shim.manager.aw.canPeek) do
      let awflit = shim.manager.aw.peek
      awid <== awflit.awid
      awaddr <== awflit.awaddr
      awsize <== (1 :: Bit (AddrWidth params)) .<<. awflit.awsize
      shim.manager.aw.consume
      write <== true

    -- Receive write data
    when (write.val .&&. shim.manager.w.canPeek) do
      let wflit = shim.manager.w.peek
      when shim.manager.b.canPut do
        shim.manager.w.consume
        -- Issue write response at end of burst
        when wflit.wlast do
          write <== false
          shim.manager.b.put
            AXI4_BFlit {
              bid = awid.val
            , bresp = resp_okay
            , buser = dontCare
            }
        -- Update data registers
        sequence_
          [ when (fromInteger i .>=. awaddr.val .&&.
                  fromInteger i .<. awaddr.val + awsize.val) do
              let j = fromInteger i - awaddr.val
              when (wflit.wstrb ! j) do
                r <== (wflit.wdata ! j)
          | (r, i) <- zip (V.toList dataRegs) [0..] ]
        -- Increment address
        awaddr <== awaddr.val + awsize.val

    -- Receive read request
    when (inv read.val .&&. shim.manager.ar.canPeek) do
      let arflit = shim.manager.ar.peek
      arcount <== arflit.arlen
      arsize <== (1 :: Bit (AddrWidth params)) .<<. arflit.arsize
      araddr <== arflit.araddr
      arid <== arflit.arid
      shim.manager.ar.consume
      read <== true

    -- Issue read response
    when (read.val .&&. shim.manager.r.canPut) do
      -- Respond with requested bytes
      let getByte i = (dataRegs ! (araddr.val + fromIntegral i)).val
      shim.manager.r.put
        AXI4_RFlit {
          rid = arid.val
        , rdata = V.map getByte V.genVec
        , rresp = resp_okay
        , rlast = arcount.val .==. 0
        , ruser = dontCare
        }
      araddr <== araddr.val + arsize.val
      arcount <== arcount.val - 1
      -- Finished reading?
      when (arcount.val .==. 0) do
        read <== false

  return shim.subordinate
