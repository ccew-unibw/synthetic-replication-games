# Synthetic replication games

## Original publication

> Hainmueller, J., & Hopkins, D. J. (2014). The Hidden American Immigration Consensus: A Conjoint Analysis of Attitudes toward Immigrants. American Journal of Political Science, 59(3), 529–548. https://doi.org/10.1111/ajps.12138

Find the original publication alongside replication data in `/material`.

## Requirements

- [uv](https://docs.astral.sh/uv/getting-started/installation/) for package management
- _R_

## Getting started

1. Initialize Python environment

   ```console
   > uv sync --frozen --dev
   ``` 

1. R setup

   ```console
   > cd material
   > Rscript -e "if (\!requireNamespace('renv', quietly = TRUE)) install.packages('renv', repos = c(CRAN = 'https://cloud.r-project.org'))"
   > Rscript -e "renv::restore()"
   ```

## Running experiments

> [!NOTE]
> We are currently using a [modified version](https://github.com/tobiasbohne-unibw/talking-to-machines/tree/add-local-inference-endpoints) of the `talkingtomachines` package that allows running experiments on any OpenAI API compatible inference server. This could be a local vLLM or Ollama instance. For compatibility reasons (vLLM doesn't support the _Responses API_), the [_Chat Completion_](https://platform.openai.com/docs/api-reference/chat) endpoints - and not the [_Responses API_](https://platform.openai.com/docs/api-reference/responses) are used whenever a custom endpoint is set in the prompt template.  

1. Launch an experiment using

   ```console
   > uv run --env-file .env talkingtomachines experiments/<experiment>/prompt.xlsx
   ```

2. Move results to respective folder

   ```console
   > mv experiment_results experiments/<experiment>/
   ```

## Running experiments on a GPU cluster (Docker)

In case you want to run the experiments on a GPU cluster with Docker support, you can reference the `docker-compose.yml` file. Adjust any of the settings as you like and run `docker compose --profile gptoss-120b up` (or `docker compose --profile llama33 up`). This will spin up a vLLM server loading the respective model defined in the profile and then runs the `talkingtomachines` script as soon as the vLLM server is available. 

## Convert LLM output to dta

The experiment run stores a JSON and CSV file on successful execution. These files need to be converted to a Stata `dta` file, which can be processed by the replication scripts. Run the following script from the root directory of this repository:

   ```console
   > uv run output_to_dta.py [PATH_TO_EXPERIMENT_JSON_OUTPUT]
   ```

The script will store the `dta` file in the same directory the input JSON lives in.

## Run replication scripts and generate plots

The main replication script `rep_estimates.do` has been ported to _R_ (`rep_estimates.R`) and extended to accept command line arguments for the input file path and naming of the output data. `rep_plots.r` was provided by the authors of the original study and only modified to also accept input and output parameters via the command line. Additionally, `comparison_plots.R` was added to create comparison plots between the original study results and the LLM experiment results.

An easy way to run the full replication including the generation of comparison plots is to execute the `generate_plots.sh` bash script:

   ```console
   > cd material
   > ./generate_plots.sh -i [PATH_TO_EXPERIMENT_DTA] -s [SUFFIX_LLM_EXPERIMENT_OUTPUTS]
   ```

This will create the replication materials and will store plots in their respective output directories, i.e. `plots_orig` (original results), `plots_exp` for the LLM results (or `plots_[SUFFIX]` if `-s` param provided), and `plots_cmp` containing the comparison plots. 