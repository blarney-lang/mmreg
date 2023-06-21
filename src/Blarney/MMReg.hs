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

-- MMReg config options
data MMRegConfig =
  MMRegConfig {
    -- Enable read access over AXI?
    enableReads :: Bool
    -- Enable write access over AXI?
  , enableWrites :: Bool
  }

-- MMReg interface
data MMReg params =
  MMReg {
    -- Memory-mapped AXI4 interface
    axi :: AXI4_Subordinate params
    -- Direct register access
  , bytes :: V.Vec (2 ^ AddrWidth params) (Reg (Bit 8))
  }

-- An MMReg is a memory-mapped register of a desired width
makeMMReg :: forall params. _ => MMRegConfig -> Module (MMReg params)
makeMMReg config = do
  -- Declare a shim to implement the interface in a simple manner
  shim <- makeAXI4BufferShim (makePipelineQueue 1)

  -- Data registers
  dataRegs :: V.Vec (2 ^ AddrWidth params) (Reg (Bit 8)) <-
    V.replicateM (makeReg dontCare)

  -- Read/write offset (bytes)
  roffset <- makeReg 0
  woffset <- makeReg 0

  -- Read count
  rcount <- makeReg 0

  always do
    -- Handle writes
    when config.enableWrites do
      when (shim.manager.aw.canPeek .&&.
              shim.manager.w.canPeek .&&.
                shim.manager.b.canPut) do
        -- Shorthands
        let awflit = shim.manager.aw.peek
        let wflit = shim.manager.w.peek
        let size = (1 :: Bit (AddrWidth params)) .<<. awflit.awsize
        -- Update data registers
        let addr = awflit.awaddr + woffset.val
        sequence_
          [ when (fromInteger i .>=. addr .&&.
                  fromInteger i .<. addr + size) do
              let j = fromInteger i - addr
              when (wflit.wstrb ! j) do
                r <== (wflit.wdata ! j)
          | (r, i) <- zip (V.toList dataRegs) [0..] ]
        -- Consume write
        shim.manager.w.consume
        -- Increment write offset
        woffset <== if wflit.wlast then 0 else woffset.val + size
        -- Issue write response at end of burst
        when wflit.wlast do
          shim.manager.b.put
            AXI4_BFlit {
              bid = awflit.awid
            , bresp = resp_okay
            , buser = dontCare
            }
          shim.manager.aw.consume

    -- Handle reads
    when config.enableReads do
      when (shim.manager.ar.canPeek .&&. shim.manager.r.canPut) do
        -- Shorthands
        let arflit = shim.manager.ar.peek
        let size = (1 :: Bit (AddrWidth params)) .<<. arflit.arsize
        let addr = arflit.araddr + roffset.val
        let done = rcount.val .==. arflit.arlen
        -- Respond with requested bytes
        let getByte i = (dataRegs ! (addr + fromIntegral i)).val
        shim.manager.r.put
          AXI4_RFlit {
            rid = arflit.arid
          , rdata = V.map getByte V.genVec
          , rresp = resp_okay
          , rlast = done
          , ruser = dontCare
          }
        -- Finished reading?
        if done
          then do
            shim.manager.ar.consume
            rcount <== 0
            roffset <== 0
          else do
            rcount <== rcount.val + 1
            roffset <== roffset.val + size

  return
    MMReg {
      axi = shim.subordinate
    , bytes = dataRegs
    }
