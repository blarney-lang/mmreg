-- Blarney imports
import Blarney
import Blarney.Stmt
import Blarney.Vector qualified as V
import Blarney.SourceSink

-- AXI4 imports
import Blarney.AXI4

-- MMReg imports
import Blarney.MMReg

-- Haskell imports
import System.Exit

makeMMRegTestBench :: Module ()
makeMMRegTestBench = do
  -- 64-bit MMReg with 32-bit data bus
  mmreg :: AXI4_Subordinate (AXI4_Params 0 3 2 0 0 0 0 0) <- makeMMReg

  runStmt do
    -- Issue write request for 6 bytes starting at address 1
    wait mmreg.aw.canPut 
    action do
      mmreg.aw.put
        AXI4_AWFlit {
          awid     = dontCare
        , awaddr   = 1
        , awlen    = 2
        , awsize   = 1
        , awburst  = dontCare
        , awlock   = dontCare
        , awcache  = dontCare
        , awprot   = dontCare
        , awqos    = dontCare
        , awregion = dontCare
        , awuser   = dontCare
        } 

    -- Write 6 bytes
    forM_ [0..2] \i -> do
      wait mmreg.w.canPut
      action do
        mmreg.w.put
          AXI4_WFlit {
            wdata = V.fromList (map fromInteger [2*i, 2*i+1, 0, 0])
          , wstrb = fromBitList [1, 1, 0, 0]
          , wlast = if i == 2 then true else false
          , wuser = dontCare
          }

    -- Wait for write response
    wait mmreg.b.canPeek
    action do
      mmreg.b.consume

    -- Issue read request for 4 bytes starting at address 3
    wait mmreg.ar.canPut
    action do
      mmreg.ar.put
        AXI4_ARFlit {
          arid     = dontCare
        , araddr   = 3
        , arlen    = 3
        , arsize   = 0
        , arburst  = dontCare
        , arlock   = dontCare
        , arcache  = dontCare
        , arprot   = dontCare
        , arqos    = dontCare
        , arregion = dontCare
        , aruser   = dontCare
        }

    -- Read 4 bytes
    forM_ [0..3] \i -> do
      wait mmreg.r.canPeek
      action do
        display (V.head mmreg.r.peek.rdata)
        mmreg.r.consume

    action do
      finish
 
-- Report test success via exit code
main :: IO ()
main = do
  output <- simulateCapture makeMMRegTestBench
  when (lines output /= ["2", "3", "4", "5"]) do
    die "Test failed: makeMMRegTestBench"
