# Soft Typing 4 PHP

## Introduction

This Experimentation Project (7.5 ECTS) was undertaken as part of my study Software Technology 
at the University of Utrecht in May and June 2011. The goal of the project is to 
statically infer types for every variable in a PHP program and to give suitable warnings 
whenever a program is provably wrong. 

This was realized by specifying the problem as a monotone framework instance and by solving
the instance using a iterative work-list algorithm. The theory behind monotone frameworks is 
described in Principles of Program Analysis by Nielson, Nielson & Hankin (NNH). 
During the execution of each transfer function constraints for the PHP expression at hand are generated, 
and the type of an expression is found by resolving these constraints. This idea was described 
by Camphuijsen in his thesis "Soft typing and analyses on PHP programs"

## Code organization

The code base is clearly separated between the monotone framework and a generic work-list algorithm
on one side and the framework instances on the other side. Beside soft typing, several other analyses were
implemented as well (*). This was done to get the boundary between framework and instances clear. 
These additional analyses all operate on the While language, a small imperative language defined by NNH. 

The soft typing analyses contains two distinct phases. In the first phase an instance of the monotone
framework is specified and solved. The result is a mapping between variable and type for any given 
program point. In the second phase this result is used to check wether these types match the types 
we expect. The second process doesn't specify or solve an monotone framework instance and could be 
considered as a post processing step. 

The following files and directories might be of special interest:

| File or directory                      | Description                                                           |
|:---------------------------------------|:----------------------------------------------------------------------|
| src/MF/Core.hs                         | Implements the work-list algorithm (look for the 'solve' function)    |
| src/MF/Languages/PHP/AG/Flow.ag        | Implements the conversion of an AST to a flow graph                   |
| src/MF/Languages/PHP/AG/Typing.ag      | Implements the constraint generation, the transfer function and       |
|                                        | specifies an instance of the monotone framework                       |
| src/MF/Languages/PHP/AG/Checking.ag    | Implements the expected constraint generation and generates warnings  |
|                                        | when the expected constraints don't match the types found by running  |
|                                        | the work-list algorithm.                                              |
| src/MF/Language/While/                 | Contains the implementation of the While language and several smaller |
|                                        | analyses. Of these analyses, the detection of sign analysis is inter- |
|                                        | procedural. The While AST is currently implemented using a GADT.      |


(*) : Currently these analyses additional analyses don't compile due to recent changes in the Flowable class

## Compiling and running 

To compile the program, simply type:

$ make

To analyze the file input.php, simply type:

$ ./run