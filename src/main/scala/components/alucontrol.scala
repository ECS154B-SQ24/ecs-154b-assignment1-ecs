// This file contains ALU control logic.

package dinocpu.components

import chisel3._
import chisel3.util._

/**
 * The ALU control unit
 *
 * Input:  aluop        Specifying the type of instruction using ALU
 *                          . 0 for none of the below
 *                          . 1 for 64-bit R-type
 *                          . 2 for 64-bit I-type
 *                          . 3 for 32-bit R-type
 *                          . 4 for 32-bit I-type
 *                          . 5 for non-arithmetic instruction types that uses ALU (auipc/jal/jarl/Load/Store)
 * Input:  funct7       The most significant bits of the instruction.
 * Input:  funct3       The middle three bits of the instruction (12-14).
 *
 * Output: operation    What we want the ALU to do.
 *
 * For more information, see Section 4.4 and A.5 of Patterson and Hennessy.
 * This is loosely based on figure 4.12
 */
class ALUControl extends Module {
  val io = IO(new Bundle {
    val aluop     = Input(UInt(3.W))
    val funct7    = Input(UInt(7.W))
    val funct3    = Input(UInt(3.W))

    val operation = Output(UInt(5.W))
  })

  io.operation := "b11111".U // Invalid

  //Your code goes here
  val	addw	=	"b0".U
  val	add	=	"b1".U
  val	subw	=	"b10".U
  val	sub	=	"b100".U
  val	mulw	=	"b101".U
  val	mul	=	"b110".U
  val	mulh	=	"b111".U
  val	mulhu	=	"b1000".U
  val	divw	=	"b1001".U
  val	divu	=	"b1010".U
  val	div	=	"b1011".U
  val	divuw	=	"b1100".U
  val	and	=	"b1101".U
  val	or	=	"b1110".U
  val	xor	=	"b1111".U
  val	sra	=	"b10000".U
  val	sraw	=	"b10001".U
  val	sll	=	"b10010".U
  val	sllw	=	"b10011".U
  val	srl	=	"b10100".U
  val	srlw	=	"b10101".U
  val	slt	=	"b10110".U
  val	sltu	=	"b10111".U
  val	mulhsu	=	"b11000".U
  val	remuw	=	"b11001".U
  val	remw	=	"b11010".U
  val	remu	=	"b11011".U
  val	rem	=	"b11100".U

  switch (io.aluop) {
    is ("h1".U) { // 64-bit R-type
      when       (io.funct7 === "b0000000".U && io.funct3 === "b000".U) {
        io.operation := add
      }.elsewhen (io.funct7 === "b0100000".U && io.funct3 === "b000".U) {
        io.operation := sub
      }.elsewhen (io.funct7 === "b0000000".U && io.funct3 === "b001".U) {
        io.operation := sll
      }.elsewhen (io.funct7 === "b0000000".U && io.funct3 === "b010".U) {
        io.operation := slt
      }.elsewhen (io.funct7 === "b0000000".U && io.funct3 === "b011".U) {
        io.operation := sltu
      }.elsewhen (io.funct7 === "b0000000".U && io.funct3 === "b100".U) {
        io.operation := xor
      }.elsewhen (io.funct7 === "b0000000".U && io.funct3 === "b101".U) {
        io.operation := srl
      }.elsewhen (io.funct7 === "b0100000".U && io.funct3 === "b101".U) {
        io.operation := sra
      }.elsewhen (io.funct7 === "b0000000".U && io.funct3 === "b110".U) {
        io.operation := or
      }.elsewhen (io.funct7 === "b0000000".U && io.funct3 === "b111".U) {
        io.operation := and
      }.elsewhen (io.funct7 === "b0000001".U && io.funct3 === "b000".U) {
        io.operation := mul
      }.elsewhen (io.funct7 === "b0000001".U && io.funct3 === "b001".U) {
        io.operation := mulh
      }.elsewhen (io.funct7 === "b0000001".U && io.funct3 === "b010".U) {
        io.operation := mulhsu
      }.elsewhen (io.funct7 === "b0000001".U && io.funct3 === "b011".U) {
        io.operation := mulhu
      }.elsewhen (io.funct7 === "b0000001".U && io.funct3 === "b100".U) {
        io.operation := div
      }.elsewhen (io.funct7 === "b0000001".U && io.funct3 === "b101".U) {
        io.operation := divu
      }.elsewhen (io.funct7 === "b0000001".U && io.funct3 === "b110".U) {
        io.operation := rem
      }.elsewhen (io.funct7 === "b0000001".U && io.funct3 === "b111".U) {
        io.operation := remu
      }
    }
    is ("h2".U) { // 64-bit I-type

    }
    is ("h3".U) { // 32-bit R-type
      when       (io.funct7 === "b0000000".U && io.funct3 === "b000".U) {
        io.operation := addw
      }.elsewhen (io.funct7 === "b0100000".U && io.funct3 === "b000".U) {
        io.operation := subw
      }.elsewhen (io.funct7 === "b0000000".U && io.funct3 === "b001".U) {
        io.operation := sllw
      }.elsewhen (io.funct7 === "b0000000".U && io.funct3 === "b101".U) {
        io.operation := srlw
      }.elsewhen (io.funct7 === "b0100000".U && io.funct3 === "b101".U) {
        io.operation := sraw
      }.elsewhen (io.funct7 === "b0000001".U && io.funct3 === "b000".U) {
        io.operation := mulw
      }.elsewhen (io.funct7 === "b0000001".U && io.funct3 === "b100".U) {
        io.operation := divw
      }.elsewhen (io.funct7 === "b0000001".U && io.funct3 === "b101".U) {
        io.operation := divuw
      }.elsewhen (io.funct7 === "b0000001".U && io.funct3 === "b110".U) {
        io.operation := remw
      }.elsewhen (io.funct7 === "b0000001".U && io.funct3 === "b111".U) {
        io.operation := remuw
      }
    }
    is ("h4".U) { // 32-bit I-type

    }
    is ("h5".U) { // non-arithmetic instruction types that uses ALU (auipc/jal/jarl/Load/Store)

    }
  }

}
