# Neutral Theory and biodiversity indictors for the post-2020 Global Biodiversity Framework

This repository includes the R-script and data needed to replicate the results of the following study:

* Buschke, F.T. Neutral theory exposes the challenge of bending the curve of biodiversity loss for the post-2020 Global Biodiversity Framework. *Unpublished manuscript*

The code was accurate as of 04 March 2021. For any queries, contact the author at `falko.buschke@gmail.com`

## Description of the code

The code to replicate the figures in the mansucript is included in a single annotated script called `Neutral_simulation.R`.

The first part of the code simulates a community of species under ecological drift. It then calculates the *Living Planet Index* for this data. To calculate the *Living Planet Index*, the code relies on the dedicated `rlpi` package. This package is not on the official CRAN repository, so it must be accessed and installed directly from the [rlpi GitHub repository](https://github.com/Zoological-Society-of-London/rlpi), which also requires the `devtools` package. The code needed to install these packages is included in the R-scripts.

The second part of the code estimates extinction probabilities by iterating a neutral model 10,000 times. The code for these iterations is in the script called `Extinction_probability.R`. This code includes a computationally exhausting process, so it was run on a cloud-based [RStudio server using Amazon Web Services](https://www.louisaslett.com/RStudio_AMI/). However, researchers hoping to only replicate the results can skip these iterations and use the outputs which are saved in the following three datafiles in the `Data` sub-directory:

* `VU.txt`
* `EN.txt`
* `CR.txt`

The outputs are saved in `.PNG`format as two deparate files called `Fig1.png` and `Fig2.png`, which are also included in the repository.
