import Blarney
import Blarney.AXI4
import Blarney.MMReg

-- 64-bit MMReg with 32-bit data bus
type MMReg_Params = AXI4_Params 0 3 4 0 0 0 0 0

-- Main
main :: IO ()
main = writeVerilogModule (makeMMReg @MMReg_Params) "MMReg" "./"
