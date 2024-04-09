<div style="text-align: center;">
        <picture>
                <img src="dalculatorLogo.png" alt="Library Banner">
        </picture>
</div>
<br>

<!-- Badge section -->
<div style="text-align: center;">
   <a href="https://github.com/onera/pml-analyzer/blob/master/README.md">
        <img alt="License LGPL" src="https://img.shields.io/badge/scala-2.13.4+-red"></a>
   <a href="https://github.com/onera/pml-analyzer/LICENSE">
        <img alt="License LGPL" src="https://img.shields.io/badge/License-LGPLv2.1-efefef"></a>
</div>
<br>

## About
This tool was created by a team of ONERA researchers (Rémi Delmas, Pierre Bieber, Christel Seguin) in the context of the MISSA European community project (http://www.missa-fp7.eu), which spanned from mid-2009 to mid-2011.
The tool has been extended in the DGAC project PHYDIAS (https://w3.onera.fr/PHYDIAS/en).
You can however send feedback, comments, suggestions or bug reports the authors by email: kevin.delmas@onera.fr, pierre.bieber@onera.fr, christel.seguin@onera.fr, and by placing the  [dalculator] tag in the title of the email
## Purpose of the tool
The purpose of the DALculator is to derive function independence requirements, DAL levels for functions, and failure probability budget by analyzing sets of minimal cut sets describing failure conditions.

## Compiling the DALculator 
### Java 8

You need a working installation of the Java Runtime Environment
version 8 (either OpenJDK or Oracle will do).  Installation procedures
may vary depending on your system (Windows, OSX, Linux), please follow
the official guidelines for your system.

### SBT

The compilation of the DALCulator can be easily performed
with [SBT](https://www.scala-sbt.org/). Installation procedures may vary depending on your system (Windows, OSX, Linux),
please follow the official guidelines for your system.

### Generate the standalone JAR with GUI
To generate the standalone JAR with the GUI run:
```sbtshell
 sbt assembly
```

## Running the DALculator

### Installing the SAT4J Pseudo-Boolean solver

The SAT4J solver can be freely obtained from the sat4j.org website. Version 2.2 or superior should be used. 
The files org.sat4j.core.jar, org.sat4j.pb.jar  and sat4j-pb.jar that come in the solver distribution archive shall be placed in the `src/resources` folder.

### Installing the WBO Pseudo-Boolean solver (optional)

The tool wbo1.4b-fixed can be obtained by contacting its author, Vasco Manquinho from INESC-ID in Portugal. 
The binary of the solver must be placed in the `src/resources` folder.

### Installing the lp_solve MILP solver (optional)

On a debian-based system, lp_solve is available in the standard package repositories can be  installed using the apt-get package installation utility.  
Just make sure that the package is installed on the system and that the lp_solve command is available in the user's $PATH.

### Run using the standalone JAR
To use the standalone JAR with the GUI run:
```shell
 java -jar [PATH_TO_JAR]
```
Note that by default the standalone JAR path is `target/scala-2.13/dalculator-assembly-X.Y.jar` (where X.Y is the version number).

## Theory behind the tool
The formal theory on which the DALculator is based is described in references:
* [Bieber, P., Delmas, R., & Seguin, C. (2011). DALculus–theory and tool for development assurance level allocation. In Computer Safety, Reliability, and Security: 30th International Conference, SAFECOMP 2011, Naples, Italy, September 19-22, 2011. Proceedings 30 (pp. 43-56). Springer Berlin Heidelberg.](https://link.springer.com/content/pdf/10.1007/978-3-642-24270-0_4.pdf)
* [Delmas, K., Chambert, L., Frazza, C., & Seguin, C. (2023, October). Optimization of Development Assurance Level Allocation. In 2023 IEEE/AIAA 42nd Digital Avionics Systems Conference (DASC) (pp. 1-10). IEEE.](https://hal.science/hal-04313961/document)