import chisel3._
import chisel3.util._

class Accelerator extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val done = Output(Bool())

    val address = Output(UInt (16.W))
    val dataRead = Input(UInt (32.W))
    val writeEnable = Output(Bool ())
    val dataWrite = Output(UInt (32.W))

  })

  val idle :: read :: write :: done :: Nil = Enum (4)

  io.done := false.B
  io.address := 0.U
  io.writeEnable := false.B
  io.dataWrite := 0.U

  //Write here your code
  val stateReg = RegInit(done)
  val iteratorReg = RegInit(0.U(10.W))
  val addressReg = RegInit(0.U(10.W))

  switch(stateReg) {
    is(write) {
      io.address := addressReg
      io.writeEnable := true.B
      io.dataWrite := 255.U
      stateReg := idle
    }
    is(idle) {
      when(iteratorReg === 399.U) {
        stateReg := done
      }.otherwise {
          stateReg := read
          
          addressReg := iteratorReg

          iteratorReg := iteratorReg + 1.U
      }
    }
    is(read) {
      io.address := addressReg
      when(io.dataRead != 0.U) {
        stateReg := write
        addressReg := iteratorReg + 399.U
      }
      .otherwise {
        stateReg := idle
      }
    }
    is(done) {
      when(io.start) {
        stateReg := idle
      }
      .otherwise {
        io.done := true.B
      }
    }
  }
}
