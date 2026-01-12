# LLM Replication materials: Team 5 (Hainmueller and Hopkins 2014)

Team 5 (alphabetical order):

- Tobias Bohne
- Laura Braun
- Daniel Mittermaier
- Christian Oswald
- David Spitzenberg

Original publication:

> Hainmueller, J., & Hopkins, D. J. (2014). The Hidden American Immigration Consensus: A Conjoint Analysis of Attitudes toward Immigrants. American Journal of Political Science, 59(3), 529–548. https://doi.org/10.1111/ajps.12138 

## Experiment setup

The original study conducts a _conjoint experiment_ assessing attitudes of US citizens towards immigrants. Participants are presented with five pairs of immigrant profiles with nine attributes (gender,
education, employment plans, job experience, profession, language skills, country of origin, reasons for applying, and prior trips to the United States.). For every immigrant profile pair, participants need to decide which immigrant should be admitted to the US and rate both profiles on a 7-point scale. The profile pairs are presented to the participants on separate pages. 

Unique immigrant profile pairs are generated based on a given set of values (see paper and the [Treatments](#treatments) section below) for the nine attributes defining an immigrant profile. A nationally representative sample of participants was surveyed using a two-wave survey approach. In the first wave, covariates, including attitudes toward immigration, were collected. After three weeks, the second wave contained the conjoint experiment and collected additional covariates. Participants were not aware that both survey waves were connected.

## Replication setup

In order to replicate the study with LLMs using the `talkingtomachines` package, we performed the following steps:

1. Extract participant profiles from the original study materials and convert them into the prompt template format
1. Generate unique immigrant profile pairs and store them as treatment objects in the prompt template
1. Set up prompts

Model settings etc. can be found in the respective prompt template Excel files. 

### Participant profiles

We decided to stick with the original participant profiles for the replication, but we only extracted profiles which contain data for the main covariates, yielding 1217 profiles (compared to 1407 participants completing the second wave in the original study). Dropping incomplete profiles is considered necessary, as the LLM needs to have enough background knowledge about a participant to be able to generate responses matching the profile of the participant.

Some variables have been transformed into more human and LLM readable formats:

- ``FB09`` (percentage of foreign born workers) converted from fractions (e.g. 0.25) to plain percentage values (e.g. 25)
- ``ppstaten`` (U.S. state) converted to upper case
- ``fisexp2`` (fiscal exposure to immigration) converted 1 to "low" and 2 to "high"
- ``OCC_*`` one-hot encoded occupation columns merged into one human-readable occupation column

Any remaining NAs were coded as "No response".

For every participant profile attribute column retained for the replication, column names were turned into questions. If the original questions were documented in the paper or appendix, they were used. If no questions were reported, or if the profile attribute was calculated by the researchers (e.g., the number of immigrants in a given ZIP code), the team crafted questions (e.g., "How many immigrants live in your ZIP code?""). All column-question pairs can be found in the respective experiment prompt template file under "profile".

The code for the profile extraction lives in `df_transformation.py`. The contents of the output file (`dedup_df.xlsx`) are manually copied to the prompt template profile section.

### Treatments

Every set of five immigrant profile pairs are treated as separate treatments that are randomly assigned to participants. Immigrant profile sets are randomly generated in `treatments.ipynb`. The possible attribute values are taken from the paper and the replication data. Constraints on professions (e.g. high-skill professions are only allowed for 2+ years of college) and application reasons (e.g. "Escape political/religious persecution" only if Feat_Country is Iraq, Sudan, or Somalia) are implemented as described in the paper. Additionally, the order of attributes is shuffled around for every respondent but stays the same within one set of five immigrant profile pairs.

Treatments are stored as JSON objects for easy reference within the prompt template:

```json
{
    "immigrant_a_1": 
    {
        "Feat_Gender": "Female", 
        "Feat_Education": "Equivalent to completing high school in the U.S.", 
        "Feat_Country": "Sudan", 
        "Feat_Language": "During admission interview, this applicant spoke fluent English", 
        "Feat_JobExperience": "More than five years", 
        "Feat_JobPlans": "Does not have a contract with a U.S. employer, but has done job interviews", 
        "Feat_JobProfession": "Waiter",
        "Feat_PriorTrips": "Entered the U.S. once before on a tourist visa", 
        "Feat_ApplicationReason": "Escape political/religious persecution", 
        "text": "Gender: Female\nEducation Level: Equivalent to completing high school in the U.S.\nCountry of Origin: Sudan\nLanguage Ability: During admission interview, this applicant spoke fluent English\nJob Experience: More than five years\nEmployment Plans: Does not have a contract with a U.S. employer, but has done job interviews\nProfession: Waiter\nPrior Trips to the U.S.: Entered the U.S. once before on a tourist visa\nReason for Application: Escape political/religious persecution"}, 
    "immigrant_b_1": { ... },
    ...
    "immigrant_b_5": { ... }
}
```

The `text` entry contains the shuffled list of immigrant attributes presented to the LLM within the prompt. The individual attributes are accessed when converting the output of the experiment back into the original data format.

The resulting treatment data is manually copied from `treatments.xlsx` into the experiment prompt template.

### Prompt setup

As this is a conjoint experiment, we assign one treatment set to one participant, which is the only member of a group (besides the facilitator). For prompting, this means that every group (i.e. every participant) is presented with an initial round of one prompt explaining the setup of the experiment. This is followed by the pair of immigrants taken from the treatment's `text` attribute and three questions as separate rounds ("If you had to choose between them, which of these two immigrants should be given priority to come to the United States to live?", "On a scale from 1 to 7, where 1 indicates that the United States should absolutely not admit the immigrant and 7 indicates that the United States should definitely admit the immigrant, how would you rate Immigrant 1?" and "Using the same scale, how would you rate immigrant 2?"). This is repeated for all five immigrant profile pairs, yielding 21 rounds of contexts and questions per group.

See the respective experiment prompt template Excel files for more details.

## LLM Experiment results

The experiment has been run three times. Initially, a subset of two profiles have been retained in the prompt template and the model was set to `gpt-5-nano` yielding `experiments/publication/experiment_results/team_5_1_20251216T180530Z.json`. As ten responses (five per participant) are not enough to run the replication scripts, two full runs with different open-weights LLMs (`gpt-oss-120b`, `llama-3.3-70b`) are executed. A [modified version](https://github.com/tobiasbohne-unibw/talking-to-machines/tree/add-local-inference-endpoints) of the `talkingtomachines` package has been developed for local inference against an OpenAI API compatible server.

<!--- TODO: update table! -->
| ID | Description | Model | Prompt file | Output |
| -- | ----------- | ----- | ----------- | ------ |
| 1 | 2 profiles | gpt-5-nano | ``experiment_prompt_template.xlsx`` | ``team_5_1_20251216T180530Z.json`` |
| 2 | full | gpt-oss-120b | ``experiment_prompt_template_gptoss.xlsx`` | ``team_5_1_20251216T180530Z.json`` |
| 3 | full | llama-3.3-70b | ``experiment_prompt_template_llama3-3.xlsx`` | ``team_5_1_20251216T180530Z.json`` |

The respective JSON output files for experiments 2 and 3 are converted back to the original data frame with `uv run output_to_dta.py [OUTPUT_JSON]` (see ``README.md``). These `.dta` files are then served as input to the `material/generate_plots.sh` (see ``README.md``), to perform the original analysis and create the corresponding plots. 