# Deep Learning Accelerator Based On Eyeriss V2 with Extend Custom RISC-V Instructions

This project is a deep learning accelerator implementation in Chisel. The architecture of the accelerator is based on Eyeriss v2.

And it will extend some custom RISC-V instructions in the near future.

## Run

After clone it, you can use `mill` in idea after this command:

```bash
mill mill.scalalib.GenIdea/idea
```

Or you can find `build.sbt` in history versions or other branches.

## TODO

You can find more at [the project page](https://github.com/SingularityKChen/dl_accelerator/projects/1).

## Known Bugs

+ Meet continuous zero \(more than three\) columns in weight matrix;
+ The head of address vector or data vector is zero column;

## [Elaboration](./dla/src)

### [Processing Element](./dla/src/pe)

This is the fundamental component of deep learning accelerator.

![Eyeriss v2 PE Architecture. The address SPad for both inAct and weight are used to store addr vector in the CSC compressed data, while the data SPad stores the data and count vectors](https://images-cdn.shimo.im/Zj7Aa6YzIis6Fhxs/image.png)

At SPad for loop, the row number of the 2D partial sum matrix is `M0`, the column number of it is `F0*N0*E`, the size of partial sum matrix is must less than the size of PSumSPad, which equals to `pSumDataSPadSize`.

The original size of 2D input activation matrix is `(R*C0, F0*N0*E)`, the size of 2D weight matrix is `(M0, R*C0)`. Due to the compressed [data format](#Data Format), both input activation matrix and weight matrix can be stored in a smaller SPad.

All three kinds of data are stored in column's order, i.e., partial sum store the first `M0` elements, then second until `F0*N0*E` elements.

![Map DNN to a Matrix Multiplication](https://raw.githubusercontent.com/SingularityKChen/PicUpload/master/img/20200305220730.png)

#### ProcessingElementControl

This is the control module of PE.

#### ProcessingElementPad

This is the Scratch Pad of input feature map, filter weight and partial sum. It contains seven stages.

#### [Scratch Pad Module](./dla/src/pe/SPadModule.scala)

This file contains the template of Scratch Pad module with simple read and write function.

#### [PEIOs](./dla/src/pe/PEIOs.scala)

This file contains all the IOs we need to use in Processing Element module. Including some sub IOs.

##### PETopDebugIO

This is used for debug at PE top level. It generates two sub IOs to get the signal through from control module and Scratch Pad module.

##### PEControlDebugIO

This is used for monitoring control module to top module.

+ peState: know the state of the state machine, idle, load or calculation;
+ doMACEnDebug: this signal can be seen as the start signal of MAC, so we need to keep an eye on it;

##### PESPadDebugIO

This is used for monitoring Scratch Pad module.

+ inActMatrixData: input activations matrix element;
+ inActMatrixRow: input activations matrix row index;
+ inActMatrixColumn: input activations matrix column index, also the column index of output partial sum;
+ inActAdrInc: true then the index of input activation address vector will increase;
+ inActDataInc: true then the index of input activation data and count vector will increase;
+ inActAdrIdx: the index number of input activation address vector;
+ weightAdrSPadReadOut: this is the current value read from weight address vector;
+ weightMatrixData: weight matrix element;
+ weightMatrixRow: weight matrix row index, also the row index of output partial sum;
+ productResult: `inActMatrixData * weightMatrixData`;
+ pSumLoad: the corresponding partial sum produced previous;
+ pSumResult: `productResult + pSumLoad`;
+ weightAdrInIdx: the weight address vector index;
+ sPadState: the state of Scratch Pad state machine, i.e., MAC process;

#### [PE Config](./dla/src/pe/PEConfig.scala)

This is the config file for PE generator.

##### SPadSizeConfig

Contains the size of five scratch pads.

##### PESizeConfig

Contains some more general configs.

+ weightHeight: the height of weight;
+ ofMapHeight: the height of output feature map (output partial sum);
+ inActDataWidth: input activations data width in scratch pad, it is the combination of 8-bit data vector and its corresponding 4-bit count vector;
+ inActAdrWidth: input activations address width in scratch pad, i.e., the width of address vector element;
+ weightDataWidth:
+ weightAdrWidth:
+ cscDataWidth: compressed sparse column data vector width, used for get the real element of inAct and weight;
+ cscCountWidth: csc count vector width;
+ psDataWidth: data width of partial sum;
+ fifoSize: the size of FIFO;
+ fifoEn: true then generate a PE with FIFOs;
+ ~~commonLenWidth: used for get the length of data vector, address vector.~~
+ ~~weightDataLenWidth: used for get the length of weight data vector;~~
+ inActZeroColumnCode: used for judge whether current inAct column is a zero column, default is the last number it can express;
+ weightZeroColumnCode: used for judge whether current weight column is a zero column, default is the last number it can express;

##### MCRENFConfig

Used for data reuse.


### [Cluster](./dla/src/cluster)

#### [Cluster Group](./dla/src/cluster/ClusterGroup.scala)

This is the top module of cluster group. It contains one GLB cluster, one Router cluster, one PE cluster.

#### [GLB Cluster](./dla/src/cluster/GLBCluster.scala)

This is the global buffer cluster module. It contains three input activation SRAM bank, four partial sum SRAM bank.

#### [PE Cluster](./dla/src/cluster/PECluster.scala)

This is the processing element cluster module. It is a PE Array which contains `peArrayColumnNum` columns and `peArrayRowNum` rows.

#### [Router Cluster](./dla/src/cluster/RouterCluster.scala)

This is the router cluster module. It contains `inActRouterNum` input activations router, `weightRouterNum` weight router, `pSumRouterNum` partial sum router.

Each router cluster not only connects to one GLB cluster, one PE cluster, but also at least one another router cluster.

##### InActRouter

This class is the generator of one input activations router.

- InIOs\(0\): the input activation comes from its corresponding input activations SRAM bank\(GLB Cluster\);
- InIOs\(1\): the input activation comes from its northern inAct router;
- InIOs\(2\): the input activation comes from its southern inAct router;
- InIOs\(3\): the input activation comes from its horizontal inAct router;
- OutIOs\(0\): send the input activation to PE Array;
- OutIOs\(1\): send the input activation to northern inAct router;
- OutIOs\(2\): send the input activation to southern inAct router;
- OutIOs\(3\): send the input activation to horizontal inAct router;
- outSelWire: routing mode:
  - 0: uni-cast
  - 1: horizontal
  - 2: vertical 
  - 3: broadcast

##### WeightRouter

This class is the generator of one weight router.

- inIOs\(0\): the weight comes from its corresponding GLB Cluster;
- inIOs\(1\): the weight comes from its only horizontal neighboring WeightRouter;
- OutIOs\(0\): send the data to its corresponding PE Array row;
- OutIOs\(1\): send the data to its only horizontal neighboring WeightRouter;
- inSelWire: 0 enables inIOs\(0\) and 1 enables inIOs\(1\)
- OutSelWire: 0 enables outIOs\(0\) and 1 enables outIOs\(1\)

##### PSumRouter

This class is the generator of one partial sum router.

- inIOs\(0\): the output partial sum computed by its corresponding PE Array column;
- inIOs\(1\): the partial sum read from its corresponding partial sum SRAM bank;
- inIOs\(2\): the partial sum transferred from its northern neighboring PSumRouter;
- OutIOs\(0\): send the partial sum to its corresponding PE Array column;
- OutIOs\(1\): send the partial sum bank to its corresponding partial sum SRAM bank;
- OutIOs\(2\): send the partial sum to its southern neighboring PSumRouter;
- inSelWire: its value enable the corresponding inIOs, i.e., 0 enables inIOs\(0\)
- outSelWire: its value enable the corresponding outIOs, i.e., 0 enables outIOs\(0\)

#### [Cluster Config](./dla/src/cluster/ClusterConfig.scala)

This file contains some basic static parameters needed in the Cluster Group.

## [Tests](./dla/tests/src)

This directory contains all the test files.

### [PE Test](./dla/tests/src/petest)

This directory contains PE test files.

#### [PE Spec Test](./dla/tests/src/petest/ProcessingElementSpecTest.scala)

This is the main body of PE test, including spec test from top level to Scratch Pad level.

#### [CSC Format Data R/W Test](./dla/tests/src/petest/SimplyCombineAddrDataSPad.scala)

This is one fundamental scratch pad module to test the read and write with CSC format data.

### [Cluster Tets](./dla/tests/src/clustertest)

This directory contains cluster test files.

#### [ClusterSpecTest](./dla/tests/src/clustertest/ClusterSpecTest.scala)

This is the main body of cluster group test. It contains the tests of PECluster, RouterCluster, GLBCluster and the top of three.

##### test the spec of GLB Cluster

This behavior contains several tests related to the GLB cluster's spec, i.e., three submodules and top spec.

##### test the spec of Processing Element Cluster

## Data Format

### Compressed Sparse Column Data Format

Compressed Sparse Column is a data format used to jump zero-element MAC.

The CSC format used in this project is a little different from the original one.

![Example of compressing sparse weights with compressed sparse column (CSC) coding](https://images-cdn.shimo.im/dwf3TL2QC4EdrmXJ/image.png)

The picture above shows the one used in Eyeriss V2, but in this project it changes.

+ I used Zero Column Code to represent a zero column instead of repeat the value of next address vector element;
+ I used the row number in original matrix instead of the real 'count' vector;

Although it will decrease the size of matrix column with a pseudo-count vector, it is much simpler.

### The Length of Address Vectors and Data Vectors

When we use CSC format data, the length of both address vectors and data vectors are variable. So we can not use the common way to stop reading or writing.

I use one zero `0` to show the end of one vector (one address vector or one data vector with count vector) at Scratch Pad level; I use two continuous zero `00` to show the end of one stream of vectors at SRAM bank level.

## Reference

\[1\]V. Sze, Y.-H. Chen, T.-J. Yang, and J. S. Emer, “Efficient Processing of Deep Neural Networks: A Tutorial and Survey,” Proc. IEEE, vol. 105, no. 12, pp. 2295–2329, Dec. 2017, doi: 10.1109/JPROC.2017.2761740.

\[2\]Y.-H. Chen, T.-J. Yang, J. Emer, and V. Sze, “Eyeriss v2: A Flexible Accelerator for Emerging Deep Neural Networks on Mobile Devices,” arXiv:1807.07928 \[cs\], May 2019.

\[3\]Y.-H. Chen, T.-J. Yang, J. Emer, and V. Sze, “Eyeriss v2: A Flexible and High-Performance Accelerator for Emerging Deep Neural Networks,” arXiv:1807.07928 \[cs\], May 2019.

\[4\]Y.-H. Chen, J. Emer, V. Sze, Y.-H. Chen, J. Emer, and V. Sze, “Eyeriss: a spatial architecture for energy-efficient dataflow for convolutional neural networks,” ACM SIGARCH Computer Architecture News, vol. 44, no. 3, pp. 367–379, Jun. 2016, doi: 10.1109/ISCA.2016.40.

\[5\]Y.-H. Chen, T. Krishna, J. S. Emer, and V. Sze, “Eyeriss: An Energy-Efficient Reconfigurable Accelerator for Deep Convolutional Neural Networks,” IEEE J. Solid-State Circuits, vol. 52, no. 1, pp. 127–138, Jan. 2017, doi: 10.1109/JSSC.2016.2616357.

\[6\] A. Waterman and H. Cook, “Chisel/FIRRTL: Home,” Chisel/FIRRTL, 2019. \[Online\]. Available: https://www.chisel-lang.org/. \[Accessed: 30-Nov-2019\].

\[7\] D. Pala, “Design and programming of a coprocessor for a RISC-V architecture,” laurea, Politecnico di Torino, 2017.

\[8\] A. Waterman, “Design of the RISC-V Instruction Set Architecture,” PhD Thesis, EECS Department, University of California, Berkeley, 2016.

\[9\] K. Asanović and D. A. Patterson, “Instruction Sets Should Be Free: The Case For RISC-V,” EECS Department, University of California, Berkeley, UCB/EECS-2014-146, Aug. 2014.

\[10\] A. Izraelevitz et al., “Reusability is FIRRTL ground: Hardware construction languages, compiler frameworks, and transformations,” in 2017 IEEE/ACM International Conference on Computer-Aided Design (ICCAD), 2017, pp. 209–216, doi: 10.1109/ICCAD.2017.8203780.

\[11\] K. Asanović et al., “The Rocket Chip Generator,” EECS Department, University of California, Berkeley, UCB/EECS-2016-17,