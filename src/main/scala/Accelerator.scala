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

  val read :: write :: done :: readRight :: readTop :: readBottom :: Nil = Enum (6)

  io.done := false.B
  io.address := 0.U
  io.writeEnable := false.B
  io.dataWrite := 0.U

  //Write here your code
  val previousPixel = RegInit(255.U(8.W))
  val stateReg = RegInit(done)
  val iteratorReg = RegInit(0.U(10.W))
  val modReg = RegInit(0.U(10.W)) // for performing modulo operations without using the actual operator
  val addressReg = RegInit(0.U(10.W))
  val writeReg = RegInit(0.U(8.W))

  switch(stateReg) {
    
    is(write) {
      io.address := addressReg + 400.U
      io.writeEnable := true.B
      
      io.dataWrite := writeReg

      writeReg := 0.U
      
      stateReg := read
    }
    
    is(read) {
      when(iteratorReg === 400.U) {
        stateReg := done
      }.otherwise {
          previousPixel := io.dataRead
          io.address := iteratorReg

          addressReg := iteratorReg

          when(iteratorReg === modReg) { // is a left edge pixel
            modReg := modReg + 20.U // change modReg to be the index of the next left edge pixel

            stateReg := write
          }
          .elsewhen(iteratorReg >= 20.U // not top edge pixel
              && iteratorReg <= 380.U // not bottom edge pixel
              && (iteratorReg + 1.U) != modReg // not right edge pixel
              && io.dataRead === 255.U // white pixel
              && previousPixel === 255.U // previous pixel was also white (skips left check)
          ) {
              stateReg := readRight
          }
          .otherwise{
            stateReg := write
          }

          iteratorReg := iteratorReg + 1.U
      }
    }

    is(readRight) {
      io.address := addressReg + 1.U
      when(io.dataRead === 255.U) {
        stateReg := readTop
      }
      .otherwise {
        stateReg := write
      }
    }

    is(readTop) {
      io.address := addressReg - 20.U
      when(io.dataRead === 255.U) {
        stateReg := readBottom
      }
      .otherwise {
        stateReg := write
      }
    }

    is(readBottom) {
      io.address := addressReg + 20.U
      when(io.dataRead === 255.U) {
        writeReg := 255.U
        stateReg := write
      }
      .otherwise {
        stateReg := write
      }
    }

    is(done) {
      when(io.start) {
        stateReg := read
      }
      .otherwise {
        io.done := true.B
      }
    }
  }
}
