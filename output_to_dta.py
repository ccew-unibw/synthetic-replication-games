import pandas as pd
import numpy as np
import argparse

from collections import defaultdict

import json

COLUMNS = ['CaseID', 'weight2', 'contest_no', 'FeatEd', 'FeatGender',
       'FeatCountry', 'FeatReason', 'FeatJob', 'FeatExp', 'FeatPlans',
       'FeatTrips', 'FeatLang', 'Chosen_Immigrant', 'Support_Admission',
       'Rating_Immigrant', 'W1_Q1', 'W1_Q2', 'W1_Q3', 'W1_Q4', 'W1_Q5',
       'W1_Q6', 'W1_Q7', 'W1_Q8a', 'W1_Q8b', 'W2_Q16', 'ppage', 'ppeducat',
       'ppethm', 'ppgender', 'ppincimp', 'ppstaten', 'Party_ID', 'Ideology',
       'Panel_Tenure_Months', 'OCC_GA', 'OCC_WA', 'OCC_NU', 'OCC_CC', 'OCC_JA',
       'OCC_CO', 'OCC_FA', 'OCC_RS', 'OCC_DR', 'OCC_CP', 'OCC_TE', 'FB09',
       'fisexp1', 'fisexp2', 'censusgroup']

PROFILE_COLUMNS = ['CaseID', 'weight2', 'W1_Q1', 'W1_Q2', 'W1_Q3', 'W1_Q4', 'W1_Q5',
       'W1_Q6', 'W1_Q7', 'W1_Q8a', 'W1_Q8b', 'W2_Q16', 'ppage', 'ppeducat',
       'ppethm', 'ppgender', 'ppincimp', 'ppstaten', 'Party_ID', 'Ideology',
       'Panel_Tenure_Months', 'OCC_GA', 'OCC_WA', 'OCC_NU', 'OCC_CC', 'OCC_JA',
       'OCC_CO', 'OCC_FA', 'OCC_RS', 'OCC_DR', 'OCC_CP', 'OCC_TE', 'FB09',
       'fisexp1', 'fisexp2', 'censusgroup']

FEAT_RENAME = {
    'Feat_Education':'FeatEd',
    'Feat_Gender':'FeatGender',
    'Feat_Country':'FeatCountry',
    'Feat_ApplicationReason':'FeatReason',
    'Feat_JobProfession':'FeatJob',
    'Feat_JobExperience':'FeatExp',
    'Feat_JobPlans':'FeatPlans',
    'Feat_PriorTrips':'FeatTrips',
    'Feat_Language':'FeatLang'
}

V_LABEL_RENAME = {
    'ZIP': 'censusgroup',
    'co1': 'FeatCountry',
    'job1': 'FeatJob',
    'W2_Q15': 'Rating_Immigrant',
    'V129_A': 'FeatLang',
    'V127_A': 'FeatPlans',
    'V126_A': 'FeatExp',
    'V124_A': 'FeatReason',
    'V121_A': 'FeatEd',
    'W1_Q1': 'W1_Q1',
    'W1_Q2': 'W1_Q2',
    'W1_Q3': 'W1_Q3',
    'W1_Q4': 'W1_Q4',
    'W2_Q16': 'W2_Q16',
    'PARTY_ID': 'Party_ID',
    'IDEOLOGY': 'Ideology',
    'PPAGE': 'ppage',
    'PPEDUCAT': 'ppeducat',
    'PPETHM': 'ppethm',
    'PPGENDER': 'ppgender',
    'PPINCIMP': 'ppincimp',
    'PPSTATEN': 'ppstaten',
    'gender': 'FeatGender',
    'prior': 'FeatTrips'
}

def adjust_label_names(df: pd.DataFrame) -> pd.DataFrame:
    # Some features have slightly different names than in the original categories
    df = df.copy()
    # FeatEd - remove . from U.S. / Equivalent to completing two years at college in the US -- Equivalent to completing two years of college in the US
    df['FeatEd'] = df['FeatEd'].str.replace('U.S.', 'US').replace('Equivalent to completing two years at college in the US', 'Equivalent to completing two years of college in the US')
    # FeatGender - lowercase
    df['FeatGender'] = df['FeatGender'].str.lower()
    # FeatReason - Reunite with family members already in U.S. -> Reunite with family members already in the U.S.
    df['FeatReason'] = df['FeatReason'].str.replace("Reunite with family members already in U.S.", "Reunite with family members already in the U.S.")
    # FeatExp
    df['FeatExp'] = (
        df['FeatExp'].str.replace('More than five years', 'More than five years of job training and experience')
        .replace('Three to five years', 'Three to five years of job training and experience')
        .replace('One to two years', 'One or two years of job training and experience')
    )
    # FeatPlans
    df['FeatPlans'] = df['FeatPlans'].str.replace('Does not have a contract with a U.S. employer, but has done job interviews', 'Does not have a contract with a U.S. employer but has done job interviews')
    # FeatTrips
    df['FeatTrips'] = df['FeatTrips'].str.replace('Entered the U.S. once before on a tourist visa', 'Entered U.S. once before on a tourist visa').replace('Spent six months with family members in the U.S.', 'Spent six months with family members in the U.S')
    # FeatLang
    df['FeatLang'] = df['FeatLang'].str.replace('During admission interview, this applicant spoke fluent English', 'During the admission interview, this applicant spoke fluent English').replace('During admission interview, this applicant spoke through an interpreter', 'During admission interview, this applicant spoke [language] and used an interpreter')
    # RatingImmigrant - the original labels are strings....
    df['Rating_Immigrant'] = df['Rating_Immigrant'].astype(str).str.replace('1', '1 - Absolutely Not Admit').replace('7', '7 - Definitely Admit')
    return df


def apply_original_categorical_scheme(df: pd.DataFrame) -> pd.DataFrame:
    """Apply original category schema to the data frame.

    Args:
        df (pd.DataFrame): DataFrame

    Returns:
        pd.DataFrame: DataFrame with original categorical dtypes
    """
    repdata = pd.read_stata('material/repdata.dta')
    repdata_cat_dtypes = {
        col: repdata[col].dtype
        for col in repdata.select_dtypes(include="category").columns
    }
    for col, cat_dtype in repdata_cat_dtypes.items():
        df[col] = df[col].astype(cat_dtype)

    return df


def retrieve_profiles_from_stata() -> pd.DataFrame:
    """Grab profile info from the original Stata file instead of parsing it from the experiment output.

    Returns:
        pd.DataFrame: Profile Dataframe
    """
    repdata = pd.read_stata('material/repdata.dta')[PROFILE_COLUMNS]
    repdata = repdata.drop_duplicates(subset="CaseID")
    return repdata

def get_stata_labels() -> tuple[dict, dict]:
    """Get Stata compatible variable and value labels from the replication .dta file.

    Returns:
        tuple[dict, dict]: variable and value label dicts
    """
    variable_labels, value_labels = {}, {}
    with pd.io.stata.StataReader('material/repdata.dta') as reader:
        variable_labels, value_labels = reader.variable_labels(), reader.value_labels()
    # up value label indices by 1 and turn into string
    return variable_labels, value_labels

def transform_value_labels(value_labels: dict) -> dict:
    return {V_LABEL_RENAME[key]: value for key, value in value_labels.items()}

def translate_profile_columns():
    experiment_template = pd.read_excel('experiments/publication/experiment_prompt_template.xlsx', sheet_name='profile')
    return {value: key for key, value in experiment_template.iloc[0].to_dict().items()}

def transform_value_columns_to_numerics(df: pd.DataFrame, value_labels: dict) -> pd.DataFrame:
    for key, values in value_labels.items():
        # Replace throws a warning for using replace to recast a variable
        df[key] = df[key].replace({v: k for k, v in values.items()}).astype(float)
    return df

if __name__ == "__main__":
    profile_column_dict = translate_profile_columns()
    parser = argparse.ArgumentParser()
    parser.add_argument("path", type=str, help="path to the experiment result json")
    args = parser.parse_args()

    fp_experiment = args.path
    # Load json
    try:
        experiment_output = json.load(open(fp_experiment, 'r'))
    except Exception as e:
        print("Couldn't load experiment output data...")
        raise e

    # Gather all required data
    treatment_data = defaultdict(list)
    choice_data = defaultdict(list)
    rating_data = defaultdict(list)

    for group_id in experiment_output["groups"]:
        respondent_data = experiment_output["groups"][group_id]
        for profile in respondent_data['profiles']:
            respondent_id = profile["ID"]
            # gather treatment data (and merge)
            for treatment_key, treatment in respondent_data['treatment'].items():
                if treatment_key == "description":
                    continue
                contest_no = treatment_key.split('_')[-1] # pair id
                immigrant_id = treatment_key.split('_')[-2] # immigrant id
                for feature_key, feature in treatment.items():
                    if feature_key == "text":
                        continue
                    treatment_data[feature_key].append(feature)
                treatment_data["CaseID"].append(respondent_id)
                treatment_data["contest_no"].append(contest_no)
                treatment_data["immigrant_id"].append(immigrant_id)
        for response in respondent_data['message_history']:
            if "response_name" not in response.keys() and "subject_id" not in response.keys():
                continue
            
            if "choice_immigrant_" in response['response_name']:
                choice = json.loads(response['respondent'])['response']
                choice_data['CaseID'].extend([response['subject_id']]*2)
                choice_data['contest_no'].extend([response['response_name'].split('_')[-1]]*2)
                choice_data['immigrant_id'].extend(["a", "b"])
                choice_data['Chosen_Immigrant'].extend([1, 0] if choice.lower() == "a" else [0, 1])
            
            if "rating_immigrant_" in response['response_name']:
                rating = int(json.loads(response['respondent'])['response'])
                contest_no = response['response_name'].split('_')[-1]
                immigrant_id = response['response_name'].split('_')[-2]
                rating_data['CaseID'].append(response['subject_id'])
                rating_data['contest_no'].append(contest_no)
                rating_data['immigrant_id'].append(immigrant_id)
                rating_data['Rating_Immigrant'].append(rating)

    # Rename Treatment cols
    treatment_data = pd.DataFrame(treatment_data).rename(columns=FEAT_RENAME)
    # Load profile data from stata
    profile_data = retrieve_profiles_from_stata()
    full = treatment_data.merge(profile_data, how="left", on="CaseID")
    choice_data = pd.DataFrame(choice_data)
    rating_data = pd.DataFrame(rating_data)
    response_data = choice_data.merge(rating_data, how="left", on=["CaseID", "contest_no", "immigrant_id"])
    full = full.merge(response_data, how="left", on=["CaseID", "contest_no", "immigrant_id"])
    # Support_Admission is a simple cutoff bool whenever the rating is higher than 4
    full['Support_Admission'] = full['Rating_Immigrant'].apply(lambda x: 1 if x > 4 else 0)
    full = full[COLUMNS]
    full = full.astype({'contest_no': np.int8})
    full = adjust_label_names(full)
    full = apply_original_categorical_scheme(full)
    variable_labels, value_labels = get_stata_labels()
    # The original labels have keys differing from
    # the column names. Hence, we need to transform them first
    value_labels = transform_value_labels(value_labels)
    # As we can not apply the proper value_labels to categorical columns
    # we have to transform them into their raw numerical representation
    # first. After that we can write the data to a Stata file.
    full = transform_value_columns_to_numerics(full, value_labels)
    full.to_stata(fp_experiment.replace('json', 'dta'), variable_labels=variable_labels, value_labels=value_labels, write_index=False)
    
                
