# Deep Learning Accelerator Based On Eyeriss V2 with Extend Custom RISC-V Instructions

This project is a deep learning accelerator implementation in Chisel. The architecture of the accelerator is based on Eyeriss v2.

And it will extend some custom RISC-V instructions in the near future.

## TODO

- [ ] Implementation of Processing Elements

- [ ] Test file of PE

- [ ] Software analysis based on Eyexam

- [ ] Implementation of NoC level

- [ ] Test of NoC level

- [ ] Implementation of Global Buffer level

- [ ] Test of GB level

- [ ] Implementation of DRAM level

- [ ] Test of DRAM level

- [ ] Custom instruction

- [ ] ...

## Elaboration

### [ProcessingElement](https://github.com/SingularityKChen/dl_accelerator/blob/master/src/main/scala/ProcessingElement.scala)

This is the fundamental component of deep learning accelerator.

#### ProcessingElementControl

This is the control module of PE.

#### ProcessingElementPad

This is the Scratch Pad of input feature map, filter weight and partial sum.

#### ProcessingElementMAC

This is the MAC module of PE.