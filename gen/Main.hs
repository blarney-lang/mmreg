import Blarney
import Blarney.AXI4
import Blarney.MMReg

-- 64-bit MMReg with 32-bit data bus
type MMReg_Params = AXI4_Params 0 3 2 0 0 0 0 0

-- Main
main :: IO ()
main = writeVerilogModule makeMMRegAXI4 "MMReg" "./"
  where
    makeMMRegAXI4 = do
      r <- makeMMReg @MMReg_Params
      return r.axi
