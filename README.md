Replication package for Haskell'22 paper Investigating Magic Numbers: Improving the Inlining Heuristic in the Glasgow Haskell Compiler

## These are the necessary files to download a given list of Hackage packages and time them with a given GHC. These scripts use a modified QuickCheck to make random test generation repeatable.
In the paper Investigating Magic Numbers: Improving the Inlining Heuristic in the Glasgow Haskell Compiler, we used
these scripts to download stackage-nightly-2020-01-31 and time a set of packages with a modified GHC 8.10.3. The data we collected and used in the paper is included in this repository, along with the R scripts used to analyze the results.

## To Build the Magic Number GHC
Follow the README in GHC_MODIFICATIONS

## To Download Hackage Projects, Remove INLINE Pragmas, and Time Original and Modified Projects
From the src folder, run <code>python3 do_pragma_comparison.py \<project-list\> \<compiler\> </code>, where the project list is the path to a newline-separated file of the projects to be built, pragmas removed, and timed. The scripts will create an EXPERIMENTS folder with an ORIGINAL_PROJECTS folder and a MODIFIED folder (containing the same projects, INLINE pragmas removed). It will additionally create a TIMES_MODIFIED folder and a TIMES_ORIGINAL_PROJECTS folder to hold timings for the projects without and with INLINE pragmas, respectively.

## To Randomize the Threshold Parameters in Modfied GHC and Time Different Configurations
After building modified GHC, run <code>python3 random_timer.py \<path-to-modified-ghc\> \<path-to-projects-folder\></code>. The script will create a RANDOM_TIMES folder in the EXPERIMENTS folder.

## To See the Visualizations for the Data Collected in the Paper
Run process_and_viz.Rmd in the VIZ folder.
