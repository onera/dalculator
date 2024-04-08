<div align="center">
        <picture>
                <img src="dalculatorLogo.png" alt="Library Banner">
        </picture>
</div>
<br>

<!-- Badge section -->
<div align="center">
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

## Installing the DALculator 
Unziping the dalculator.zip archive will create a folder named “dalculator”  which contains the jar files needed to run the DALculator GUI and core algorithms. 

However, the DALculator needs third party constraint solvers to be installed on the system to function properly: At least one of the SAT4J or WBO Pseudo-boolean solvers is needed to compute the function independence and DAL allocation (having both is better), and the lp_solve MILP solver is needed to solve the budget allocation problem. Directions on how to obtain and install these third party solvers follow in the next subsection.

Once third party solvers are set up properly, you can run the DALculator by opening a terminal, changing the current working directory to the dalculator directory created when unzipping the archive, and running the command “./run_dalculator”, which will launch the GUI. 

### 1.1  Installing the SAT4J Pseudo-Boolean solver
The SAT4J solver can be freely obtained from the sat4j.org website. Version 2.2 or superior should be used. The files org.sat4j.core.jar, org.sat4j.pb.jar  and sat4j-pb.jar that come in the solver distribution archive shall be placed in the dalculator/lib folder.
### 1.2  Installing the WBO Pseudo-Boolean solver
The tool wbo1.4b-fixed can be obtained by contacting its author, Vasco Manquinho from INESC-ID in Portugal. Full contact details are available here : Contact. The binary of  the solver must be placed in the dalculator/lib folder.
### 1.3  Installing the lp_solve MILP solver
On a debian-based system, lp_solve is available in the standard package repositories can be  installed using the apt-get package installation utility.  Just make sure that the package is installed on the system and that the lp_solve command is available in the user's $PATH.

## Theory behind the tool
The formal theory on which the DALculator is based is described in references:
* [Bieber, P., Delmas, R., & Seguin, C. (2011). DALculus–theory and tool for development assurance level allocation. In Computer Safety, Reliability, and Security: 30th International Conference, SAFECOMP 2011, Naples, Italy, September 19-22, 2011. Proceedings 30 (pp. 43-56). Springer Berlin Heidelberg.](https://link.springer.com/content/pdf/10.1007/978-3-642-24270-0_4.pdf)
* [Delmas, K., Chambert, L., Frazza, C., & Seguin, C. (2023, October). Optimization of Development Assurance Level Allocation. In 2023 IEEE/AIAA 42nd Digital Avionics Systems Conference (DASC) (pp. 1-10). IEEE.](https://hal.science/hal-04313961/document)