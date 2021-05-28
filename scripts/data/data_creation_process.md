<script type="text/javascript"
        src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-AMS_CHTML"></script>

# Data Creation Process

#### General

This document provides a thorough walkthrough of how datasets were created for my master thesis. The programming language used is `Python` with good support from the package `Pandas`.
However, it should not be necessary with any prior knowledge of programming in `Python` to get the main message of this document. This document is therefore intended for those who want to understand how the datasets used in this thesis were created, and for those who wishes to use the code to create/recreate datasets. 

The walkthrough is given in 13 steps, which may sound a lot. However, most steps will be a quick read, and there is a lot of output included which makes the document lengthy. The 13 steps are 

1. **Reading the GED dataset**
2. **Removing unneccessary columns**
3. **Removing rows with specified type_of_violence values**
4. **Removing rows with specified date_prec values**
5. **Introducing variable type_of_conflict into the dataset**
6. **Removing rows with specified type_of_conflict values**
7. **Select countries of interest**
8. **Update the `best` estimate** 
9. **Aggregate the data based on new column ref_time**
10. **Aggregate data by year and week**
11. **Introducing `v2x_polyarchy` to the datasets**
12. **Introducing World Bank indicators to the datasets**
13. **Write data to .csv**

The structure of each step is divided in 3. First, details of the step is provided. Second, the code for handling the step's description can optionally be shown. And lastly, the first 3 rows in the dataset(s) are shown when the current step has been executed. Why only 3 rows? Well, it's a bit cumbersome to work with the document showing more rows. Hence, not every step will illustrate what has happened to the dataset. 

By the end of all steps, a reference list to citations in the text and datasets are given.

#### Comments about the code

I decided to implement a class structure for processing the data. This is because it mainly gives a clear structure of the code, is easy to extend (something I have already done several times), and is somewhat flexible to use. The class is called **AggregateConflictData**, and contains methods for processing the Georeferenced Event Dataset (GED). 

In the code section of each step, the method of the class is shown first followed by the call of that method separated by multiple #'s. Code from the method is obtained in *agg_conflict_data.py*, while calls to the method are obtained from *data_creation_process_example.py*. The code is developed with **Python 3.9.1**, using the following packages (and their versions) 
```
pandas==1.2.2
numpy==1.20.1
wbdata==0.3.0
```

 
### Step 1: Reading the GED dataset 
The first step was simply to read in the GED dataset.

---
<details>
  <summary>Click to see code!</summary>

```python
class AggregateConflictData:
    """Filters and compiles datasets to create unique dataset(s).

    The class creates one or multiple datasets of conflict data. It uses 3 different datasets which must be stored locally.
    These locally stored datasets are;
    - UCDP GED version 20.1 
    - UCDP/PRIO ACD version 20.1
    - V-DEM: Country-Year Core version 10 
    Additionally, the class uses an API from the World Bank. Hence, internet connection is required when using this script.

    The methods in the class represent different filtering/compiling/storing steps to be utilized. However, these methods 
    should be executed in a specific order to obtain desired results. Hence, the class is quite rigid. The point of using 
    a class is to collect a bunch of functions doing simple/complicated operations, and easily have the possibility of
    extending the class.  

    """
    def __init__(self, ged_data_path):
        """Read UCDP GED data specified from path.

        The method reads and stores UCDP GED data as df from inputted path, and stores the first and last date in df. 

        Args: 
           ged_data_path (str): Relative or absolute path to UCDP GED data.

        """
        self.df = pd.read_csv(ged_data_path)
        self.first_date = pd.to_datetime(self.df.date_start.min(), format='%Y-%m-%d') # used in basic_handling_of_dates()
        self.last_date = pd.to_datetime(self.df.date_end.max(), format='%Y-%m-%d') # used in basic_handling_of_dates()

#################################################

from agg_conflict_data import AggregateConflictData

import os
os.chdir('../../') # Go to root directory
os.chdir('./data/') # Go to data directory

data = AggregateConflictData(ged_data_path='./raw_data/ged201.csv') 
```
</details>

---
**Output:**

|    |     id | relid              |   year |   active_year | code_status   |   type_of_violence |   conflict_dset_id |   conflict_new_id | conflict_name           |   dyad_dset_id |   dyad_new_id | dyad_name                                                     |   side_a_dset_id |   side_a_new_id | side_a                    |   side_b_dset_id |   side_b_new_id | side_b                            |   number_of_sources | source_article                                                                                                                                                                                                                                                         | source_office                                          | source_date                      | source_headline                                                                                                                                                          | source_original                        |   where_prec | where_coordinates   | where_description      | adm_1              | adm_2          |   latitude |   longitude | geom_wkt                    |   priogrid_gid | country     |   country_id | region   |   event_clarity |   date_prec | date_start              | date_end                |   deaths_a |   deaths_b |   deaths_civilians |   deaths_unknown |   best |   high |   low |   gwnoa |   gwnob |
|---:|-------:|:-------------------|-------:|--------------:|:--------------|-------------------:|-------------------:|------------------:|:------------------------|---------------:|--------------:|:--------------------------------------------------------------|-----------------:|----------------:|:--------------------------|-----------------:|----------------:|:----------------------------------|--------------------:|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:-------------------------------------------------------|:---------------------------------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:---------------------------------------|-------------:|:--------------------|:-----------------------|:-------------------|:---------------|-----------:|------------:|:----------------------------|---------------:|:------------|-------------:|:---------|----------------:|------------:|:------------------------|:------------------------|-----------:|-----------:|-------------------:|-----------------:|-------:|-------:|------:|--------:|--------:|
|  0 | 244657 | IRQ-2017-1-524-322 |   2017 |             1 | Clear         |                  1 |                259 |               259 | Iraq: Government        |            524 |           524 | Government of Iraq - IS                                       |              116 |             116 | Government of Iraq        |              234 |             234 | IS                                |                   3 | "Agence France Presse,2017-08-01,Attackers target Shiite mosque in Afghanistan's Herat";"Agence France Presse,2017-08-01,At least 20 killed in Shiite mosque attack in Afghanistan's Herat";"Pajhwok News,2017-07-31,Assailants among 6 killed in Iraq Embassy attack" | Agence France Presse;Agence France Presse;Pajhwok News | 2017-08-01;2017-08-01;2017-07-31 | Attackers target Shiite mosque in Afghanistan's Herat;At least 20 killed in Shiite mosque attack in Afghanistan's Herat;Assailants among 6 killed in Iraq Embassy attack | IS, interior ministry, security source |            1 | Kabul city          | Iraqi embassy in Kabul | Kabul province     | Kabul district |    34.5311 |     69.1628 | POINT (69.162796 34.531094) |         179779 | Afghanistan |          700 | Asia     |               1 |           1 | 2017-07-31 00:00:00.000 | 2017-07-31 00:00:00.000 |          0 |          4 |                  0 |                2 |      6 |      6 |     6 |     645 |     nan |
|  1 | 132140 | AFG-1989-1-411-2   |   1989 |             1 | Clear         |                  1 |                333 |               333 | Afghanistan: Government |            724 |           724 | Government of Afghanistan - Jam'iyyat-i Islami-yi Afghanistan |              130 |             130 | Government of Afghanistan |              292 |             292 | Jam'iyyat-i Islami-yi Afghanistan |                  -1 | The Times 13 Jan 1989 "Missiles and tea breaks for rebels;Afghanistan                                                                                                                                                                                                  | nan                                                    | nan                              | nan                                                                                                                                                                      | Rebel source                           |            4 | Nangarhar province  | Nangarhar province     | Nangarhar province | nan            |    34.3333 |     70.4167 | POINT (70.416670 34.333330) |         179061 | Afghanistan |          700 | Asia     |               1 |           3 | 1989-01-07 00:00:00.000 | 1989-01-13 00:00:00.000 |          6 |          0 |                  0 |                0 |      6 |      6 |     6 |     700 |     nan |
|  2 | 130364 | AFG-1989-1-411-37  |   1989 |             1 | Clear         |                  1 |                333 |               333 | Afghanistan: Government |            724 |           724 | Government of Afghanistan - Jam'iyyat-i Islami-yi Afghanistan |              130 |             130 | Government of Afghanistan |              292 |             292 | Jam'iyyat-i Islami-yi Afghanistan |                  -1 | R 18 Jan 1989 "KABUL REPORTS HUNDREDS OF REBEL CASUALTIES IN KUNDUZ PROVINCE.                                                                                                                                                                                          | nan                                                    | nan                              | nan                                                                                                                                                                      | Kabul radio (monitored in Islamabad)   |            4 | Kunduz province     | Kunduz province        | Kunduz province    | nan            |    36.75   |     68.75   | POINT (68.750000 36.750000) |         182658 | Afghanistan |          700 | Asia     |               2 |           2 | 1989-01-15 00:00:00.000 | 1989-01-18 00:00:00.000 |          0 |          0 |                  0 |                4 |      4 |      4 |     0 |     700 |     nan |

### Step 2: Removing unneccessary columns 
We removed all columns with the following labels 
```
'id', 'year', 'active_year', 'relid', 'code_status', 'conflict_dset_id',
'dyad_dset_id', 'side_a_dset_id', 'side_b_dset_id', 'where_description',
'conflict_name', 'dyad_new_id', 'dyad_name', 'side_a_new_id', 'gwnoa',
'side_a', 'side_b_new_id', 'gwnob', 'side_b', 'number_of_sources',
'source_article', 'source_office', 'source_date', 'source_headline',
'source_original', 'where_prec', 'where_coordinates', 'adm_1', 'adm_2',
'latitude', 'longitude', 'geom_wkt', 'priogrid_gid', 'country', 'region', 'event_clarity'
```
This was to make the data a bit more clear to work with, since we are primarily interested in the number of people killed. 

---
<details>
    <summary>Click to see code!</summary>

```python
    def drop_columns(self, col_ls):
        """Drops/removes columns in df specified in inputted list.

        Args: 
           col_ls (list): A list of str's representing columns in df.

        """
        self.df = self.df.drop(columns=col_ls) 

###################################################################  

col_to_drop = ['id', 'year', 'active_year', 'relid', 'code_status', 'conflict_dset_id',
               'dyad_dset_id', 'side_a_dset_id', 'side_b_dset_id', 'where_description',
               'conflict_name', 'dyad_new_id', 'dyad_name', 'side_a_new_id', 'gwnoa',
               'side_a', 'side_b_new_id', 'gwnob', 'side_b', 'number_of_sources',
               'source_article', 'source_office', 'source_date', 'source_headline',
               'source_original', 'where_prec', 'where_coordinates', 'adm_1', 'adm_2',
               'latitude', 'longitude', 'geom_wkt', 'priogrid_gid', 'country',
               'region', 'event_clarity']

data.drop_columns(col_to_drop)
```
</details>

---
**Output:**

|    |   type_of_violence |   conflict_new_id |   country_id |   date_prec | date_start              | date_end                |   deaths_a |   deaths_b |   deaths_civilians |   deaths_unknown |   best |   high |   low |
|---:|-------------------:|------------------:|-------------:|------------:|:------------------------|:------------------------|-----------:|-----------:|-------------------:|-----------------:|-------:|-------:|------:|
|  0 |                  1 |               259 |          700 |           1 | 2017-07-31 00:00:00.000 | 2017-07-31 00:00:00.000 |          0 |          4 |                  0 |                2 |      6 |      6 |     6 |
|  1 |                  1 |               333 |          700 |           3 | 1989-01-07 00:00:00.000 | 1989-01-13 00:00:00.000 |          6 |          0 |                  0 |                0 |      6 |      6 |     6 |
|  2 |                  1 |               333 |          700 |           2 | 1989-01-15 00:00:00.000 | 1989-01-18 00:00:00.000 |          0 |          0 |                  0 |                4 |      4 |      4 |     0 |


### Step 3: Removing rows with specified type_of_violence values
The `type_of_violence` column represents the type of UCDP conflict [(UCDP GED Codebook)][1]. The categorical variable takes the following values

- 1: state-based conflict
- 2: non-state conflict
- 3: one-sided violence

We removed rows with `type_of_violence` equal to 2 or 3, meaning that the only remaining `type_of_violence` in the dataset is 1: state-based conflict.

---
<details>
    <summary>Click to see code!</summary>

```python
    def remove_type_of_violence(self, type_of_violence_ls):
        """Removes rows in df with type_of_violence equal to entries in inputted list.

        Args:
           type_of_violence_ls (list): A list of integers representing type_of_violence levels.

        """        
        for violence_type in type_of_violence_ls: 
            self.df = self.df.drop(self.df[self.df.type_of_violence == violence_type].index)

######################################################################

data.remove_type_of_violence(type_of_violence_ls=[2, 3]) 
```
</details>
 
 ---
**Output:**

|    |   type_of_violence |   conflict_new_id |   country_id |   date_prec | date_start              | date_end                |   deaths_a |   deaths_b |   deaths_civilians |   deaths_unknown |   best |   high |   low |
|---:|-------------------:|------------------:|-------------:|------------:|:------------------------|:------------------------|-----------:|-----------:|-------------------:|-----------------:|-------:|-------:|------:|
|  0 |                  1 |               259 |          700 |           1 | 2017-07-31 00:00:00.000 | 2017-07-31 00:00:00.000 |          0 |          4 |                  0 |                2 |      6 |      6 |     6 |
|  1 |                  1 |               333 |          700 |           3 | 1989-01-07 00:00:00.000 | 1989-01-13 00:00:00.000 |          6 |          0 |                  0 |                0 |      6 |      6 |     6 |
|  2 |                  1 |               333 |          700 |           2 | 1989-01-15 00:00:00.000 | 1989-01-18 00:00:00.000 |          0 |          0 |                  0 |                4 |      4 |      4 |     0 |

### Step 4: Removing rows with specified date_prec values
The `date_prec` column tells us how precise the information is about the date of an event [(UCDP GED Codebook)][1]. The categorical variable takes the following values

- 1: exact date of event is known
- 2: the date of the event is known only within a 2-6 day range
- 3: only the week of the event is known
- 4: the date of the event is known only within an 8-30 day range
or only the month when the event has taken place is known
- 5: the date of the event is known only within a range longer
than one month but not more than one calendar year

We removed rows with `date_prec` equal to 4 or 5, meaning that our dataset only contains events with a maximum uncertainty of 1 week for when events happend. 

---
<details>
    <summary>Click to see code!</summary>

```python
    def remove_date_prec(self, precision_ls):
        """Removes rows in df with date_prec equal to entries in inputted list.

        Args:
           precision_ls (list): A list of integers representing date_prec levels.

        """
        for precision in precision_ls:
            self.df = self.df.drop(self.df[self.df.date_prec == precision].index)  

#############################################################################

data.remove_date_prec(precision_ls=[4, 5]) 
```
</details>

---
**Output:**

|    |   type_of_violence |   conflict_new_id |   country_id |   date_prec | date_start              | date_end                |   deaths_a |   deaths_b |   deaths_civilians |   deaths_unknown |   best |   high |   low |
|---:|-------------------:|------------------:|-------------:|------------:|:------------------------|:------------------------|-----------:|-----------:|-------------------:|-----------------:|-------:|-------:|------:|
|  0 |                  1 |               259 |          700 |           1 | 2017-07-31 00:00:00.000 | 2017-07-31 00:00:00.000 |          0 |          4 |                  0 |                2 |      6 |      6 |     6 |
|  1 |                  1 |               333 |          700 |           3 | 1989-01-07 00:00:00.000 | 1989-01-13 00:00:00.000 |          6 |          0 |                  0 |                0 |      6 |      6 |     6 |
|  2 |                  1 |               333 |          700 |           2 | 1989-01-15 00:00:00.000 | 1989-01-18 00:00:00.000 |          0 |          0 |                  0 |                4 |      4 |      4 |     0 |

### Step 5: Introducing variable type_of_conflict into the dataset
The variable `conflict_new_id` is a unique conflict identification code for each individual conflict in the dataset [(UCDP GED Codebook)][1]. In another dataset, the **UCDP/PRIO Armed Conflict Dataset** (ACD) we can find the same variable under the name `conflict_id` [(UCDP/PRIO ACD Codebook)][2] with much more information related to the specific conflict. Among that information is the `type_of_conflict` variable, representing four different types of conflict 

- 1 = extrasystemic (between a state and a non-state group
outside its own territory, where the government side is
fighting to retain control of a territory outside the state
system)
- 2 = interstate 
- 3 = intrastate (side A is always a government; side B is
always one or more rebel groups; there is no
involvement of foreign governments with troops, i.e.
there is no side_a_2nd or side_b_2nd coded)
- 4 = internationalized intrastate (side A is always a
government; side B is always one or more rebel
groups; there is involvement of foreign
governments with troops, i.e. there is at least ONE
side_a_2nd or side_b_2nd coded)

We introduced `type_of_conflict` from the ACD into the GED using the mutual variable `conflict_new_id`/`conflict_id`. This procedure is called mapping. 

---
<details>
    <summary>Click to see code!</summary>

```python
    def map_type_of_conflict_to_conflict_id(self, acd_data_path):
        """Maps column type_of_conflict from UCDP/PRIO ACD data (inputted path) into df.

        The method uses the common column conflict_id from df and UCDP/PRIO ACD (inputted path)
        to insert the column type_of_conflict into df. 

        Args:
           acd_data_path (str): Absolute or relative path to UCDP/PRIO ACD data.

        """
        df = self.df          
        acd = pd.read_csv(acd_data_path)
        id_type_dict = dict(zip(acd.conflict_id, acd.type_of_conflict))
        df['type_of_conflict'] = df['conflict_new_id'].map(id_type_dict)
        self.df = df

#######################################################################

data.map_type_of_conflict_to_conflict_id(acd_data_path="./raw_data/ucdp-prio-acd-201.csv")
```
</details>

---
**Output:**

|    |   type_of_violence |   conflict_new_id |   country_id |   date_prec | date_start              | date_end                |   deaths_a |   deaths_b |   deaths_civilians |   deaths_unknown |   best |   high |   low |   type_of_conflict |
|---:|-------------------:|------------------:|-------------:|------------:|:------------------------|:------------------------|-----------:|-----------:|-------------------:|-----------------:|-------:|-------:|------:|-------------------:|
|  0 |                  1 |               259 |          700 |           1 | 2017-07-31 00:00:00.000 | 2017-07-31 00:00:00.000 |          0 |          4 |                  0 |                2 |      6 |      6 |     6 |                  4 |
|  1 |                  1 |               333 |          700 |           3 | 1989-01-07 00:00:00.000 | 1989-01-13 00:00:00.000 |          6 |          0 |                  0 |                0 |      6 |      6 |     6 |                  4 |
|  2 |                  1 |               333 |          700 |           2 | 1989-01-15 00:00:00.000 | 1989-01-18 00:00:00.000 |          0 |          0 |                  0 |                4 |      4 |      4 |     0 |                  4 |

### Step 6: Removing rows with specified type_of_conflict values
We removed rows with `type_of_conflict` equal to 1 or 2. Hence, we removed conflicts which were of type extrasystemic and interstate, and kept conflicts of type intrastate and internationalized intrastate.

---
<details>
    <summary>Click to see code!</summary>

```python
    def remove_type_of_conflict(self, type_of_conflict_ls):
        """Removes rows in df with type_of_conflict equal to entries in inputted list.

        Args: 
           type_of_conflict_ls (list): A list of integers representing type_of_conflict levels.

        """  
        for conflict_type in type_of_conflict_ls:
            self.df = self.df.drop(self.df[self.df.type_of_conflict == conflict_type].index)  

########################################################

data.remove_type_of_conflict(type_of_conflict_ls=[1, 2])
```
</details>

---
**Output:**

|    |   type_of_violence |   conflict_new_id |   country_id |   date_prec | date_start              | date_end                |   deaths_a |   deaths_b |   deaths_civilians |   deaths_unknown |   best |   high |   low |   type_of_conflict |
|---:|-------------------:|------------------:|-------------:|------------:|:------------------------|:------------------------|-----------:|-----------:|-------------------:|-----------------:|-------:|-------:|------:|-------------------:|
|  0 |                  1 |               259 |          700 |           1 | 2017-07-31 00:00:00.000 | 2017-07-31 00:00:00.000 |          0 |          4 |                  0 |                2 |      6 |      6 |     6 |                  4 |
|  1 |                  1 |               333 |          700 |           3 | 1989-01-07 00:00:00.000 | 1989-01-13 00:00:00.000 |          6 |          0 |                  0 |                0 |      6 |      6 |     6 |                  4 |
|  2 |                  1 |               333 |          700 |           2 | 1989-01-15 00:00:00.000 | 1989-01-18 00:00:00.000 |          0 |          0 |                  0 |                4 |      4 |      4 |     0 |                  4 |

### Step 7: Select countries of interest
In this step we select one or several countries that we want to obtain individual datasets for. As an example we will choose Colombia and the Democratic Republic of the Congo (DR Congo). In order to select these countries we must provide each country's `country_id`, which is a variable in the GED. The variable `country_id` is the Gleditsch and Ward number of the country in which the event takes place [(UCDP GED Codebook)][1]. For Colombia and DR Congo, the `country_id` is 100 and 490, respectively. These id's can easily be obtained by visiting https://ucdp.uu.se/exploratory and search for a country. The id is displayed in the url when you have entered some specific country's webpage; for example https://ucdp.uu.se/country/100 for Colombia. 

When executing this step we split up the data from the last step into two datasets (for this particular example; one for Colombia and one for DR Congo), and discard all other data. The order of splitting is significant for later operations, so keep this in mind. In this example, the order is Colombia and then DR Congo. From now on, the output will reflect the two datasets with Colombia displayed first and then DR Congo.    

---
<details>
    <summary>Click to see code!</summary>

```python
    def select_countries(self, country_ls):
        """Stores individual dataframes representing countries equal to entries in inputted list.

        The method separates df into individual dataframes representing countries specified in
        inputted list country_ls, and stores these individual dataframes in list country_dfs.

        Args:
           country_ls (list): A list of integers representing country_id in UCDP GED. 

        """
        country_dfs = []
        for country in country_ls:
            country_dfs.append(self.df[(self.df['country_id'] == country)]) 
        self.country_dfs = country_dfs

#######################################################################

data.select_countries(country_ls=[100, 490])
```
</details>

---
**Output:**

|       |   type_of_violence |   conflict_new_id |   country_id |   date_prec | date_start              | date_end                |   deaths_a |   deaths_b |   deaths_civilians |   deaths_unknown |   best |   high |   low |   type_of_conflict |
|------:|-------------------:|------------------:|-------------:|------------:|:------------------------|:------------------------|-----------:|-----------:|-------------------:|-----------------:|-------:|-------:|------:|-------------------:|
| 48736 |                  1 |               289 |          100 |           1 | 1989-02-05 00:00:00.000 | 1989-02-05 00:00:00.000 |          0 |          3 |                  0 |                0 |      3 |      3 |     3 |                  3 |
| 48737 |                  1 |               289 |          100 |           1 | 1989-02-10 00:00:00.000 | 1989-02-10 00:00:00.000 |          5 |          0 |                  0 |                0 |      5 |      5 |     5 |                  3 |
| 48738 |                  1 |               289 |          100 |           2 | 1989-03-22 00:00:00.000 | 1989-03-27 00:00:00.000 |          2 |          0 |                  1 |                0 |      3 |      3 |     3 |                  3 |

|       |   type_of_violence |   conflict_new_id |   country_id |   date_prec | date_start              | date_end                |   deaths_a |   deaths_b |   deaths_civilians |   deaths_unknown |   best |   high |   low |   type_of_conflict |
|------:|-------------------:|------------------:|-------------:|------------:|:------------------------|:------------------------|-----------:|-----------:|-------------------:|-----------------:|-------:|-------:|------:|-------------------:|
| 53864 |                  1 |               283 |          490 |           1 | 1996-10-18 00:00:00.000 | 1996-10-18 00:00:00.000 |          0 |          0 |                  0 |                3 |      3 |      3 |     3 |                  3 |
| 53865 |                  1 |               283 |          490 |           2 | 1996-10-18 00:00:00.000 | 1996-10-20 00:00:00.000 |          0 |          0 |                  0 |               55 |     55 |     78 |    55 |                  3 |
| 53866 |                  1 |               283 |          490 |           1 | 1996-10-19 00:00:00.000 | 1996-10-19 00:00:00.000 |          0 |          0 |                  0 |               28 |     28 |     28 |    28 |                  3 |                  3 |

### Step 8: Update the `best` estimate
There exists rows in the GED data where we have non-zero values for `low` and `high`, and zero in `best`. This is due to stricter requirement of sources for death counts in the `best` column, compared to `low` and `high` column. When such cases arises, we want to include information from the `low` and `best` column. So, whenever row \\(i\\) contains the following values
 \\[\texttt{best}_i = 0 \qquad \texttt{low}_i\geq 0 \qquad \texttt{high}_i>0 \\] We update our \\(\texttt{best}_i\\) estimate according to \\[ \texttt{best}_i = \text{trunc}\left(\frac{\texttt{low}_i + \texttt{high}_i}{2}\right) \\] where \\(\text{trunc}\\) truncates the value within the parentheses.

---
<details>
    <summary>Click to see code!</summary>

```python
    def add_low_and_high_to_best_if_best_is_zero(self):
        """If entries in country_dfs have rows with low >= 0, best = 0 and high > 0, let best=(low+high)/2 rounded down."""
        tmp_ls = []
        for country in self.country_dfs:
            country_copy = country.copy() 
            bool_series = ((country_copy.low >= 0) & (country_copy.best == 0) & (country_copy.high > 0))
            del country_copy
            country.loc[bool_series, "best"] = ((country.loc[bool_series, "low"] + country.loc[bool_series, "high"])/2).apply(lambda y: int(np.floor(y)))
            tmp_ls.append(country)
        self.country_dfs = tmp_ls

#######################################################################

data.add_low_and_high_to_best_if_best_is_zero()
```
</details>

---
**Output:**

|       |   type_of_violence |   conflict_new_id |   country_id |   date_prec | date_start              | date_end                |   deaths_a |   deaths_b |   deaths_civilians |   deaths_unknown |   best |   high |   low |   type_of_conflict |
|------:|-------------------:|------------------:|-------------:|------------:|:------------------------|:------------------------|-----------:|-----------:|-------------------:|-----------------:|-------:|-------:|------:|-------------------:|
| 48736 |                  1 |               289 |          100 |           1 | 1989-02-05 00:00:00.000 | 1989-02-05 00:00:00.000 |          0 |          3 |                  0 |                0 |      3 |      3 |     3 |                  3 |
| 48737 |                  1 |               289 |          100 |           1 | 1989-02-10 00:00:00.000 | 1989-02-10 00:00:00.000 |          5 |          0 |                  0 |                0 |      5 |      5 |     5 |                  3 |
| 48738 |                  1 |               289 |          100 |           2 | 1989-03-22 00:00:00.000 | 1989-03-27 00:00:00.000 |          2 |          0 |                  1 |                0 |      3 |      3 |     3 |                  3 |

|       |   type_of_violence |   conflict_new_id |   country_id |   date_prec | date_start              | date_end                |   deaths_a |   deaths_b |   deaths_civilians |   deaths_unknown |   best |   high |   low |   type_of_conflict |
|------:|-------------------:|------------------:|-------------:|------------:|:------------------------|:------------------------|-----------:|-----------:|-------------------:|-----------------:|-------:|-------:|------:|-------------------:|
| 53864 |                  1 |               283 |          490 |           1 | 1996-10-18 00:00:00.000 | 1996-10-18 00:00:00.000 |          0 |          0 |                  0 |                3 |      3 |      3 |     3 |                  3 |
| 53865 |                  1 |               283 |          490 |           2 | 1996-10-18 00:00:00.000 | 1996-10-20 00:00:00.000 |          0 |          0 |                  0 |               55 |     55 |     78 |    55 |                  3 |
| 53866 |                  1 |               283 |          490 |           1 | 1996-10-19 00:00:00.000 | 1996-10-19 00:00:00.000 |          0 |          0 |                  0 |               28 |     28 |     28 |    28 |                  3 |


### Step 9: Aggregate the data based on new column ref_time
In order to aggregate the current datasets, we need to have a reference time and resample the data based on this time first. Currently, we have two columns representing dates linked to an event 

- `date_start`: The earliest possible date when the event has taken place.
- `date_end`: The last possible date when the event has taken place

According to Håvard Nygård, a former researcher at PRIO, one way of selecting a singular time (reference time) an event is linked to is by choosing the date between `date_start` and `date_end`. Another way to choose the reference time is by selecting the last date, but we will stick with the first approach. More accurately, the reference time in row \\(i\\) is computed as 
\\[\texttt{ref\_time}_i = \texttt{date\_start}_i + (\texttt{date\_end}_i - \texttt{date\_start}_i)/2 \\]
For this computation to work, \\(\texttt{date\_start}_i\\) and \\(\texttt{date\_end}_i\\) must be on the format yyyy-mm-dd hh:mm:ss.

When selecting a reference time for an event, a side-effect involves events sharing reference time. Resampling the data based on the __date__ in `ref_time`, results in summing all columns which share the reference date. Therefore, we lose interpretability in the following columns 
```
'type_of_violence', 'conflict_new_id', 'country_id', 'date_prec', 'type_of_conflict'
``` 
Hence, these columns are removed before calculating the reference time. When the reference time is calculated, we also remove `date_start` and `date_end`. Additionally, the interpretation of `best` is now changed to meaning the best estimate of deaths recorded at an estimated reference time and can no longer be exclusively linked to an event. 

Furthermore, it is important to mention that in this step we also introduced two dummy rows, containing the desired start time and end time of the datasets, along with 0 for all death columns. (These rows are introduced if the desired start and end time does not exist in the country specific dataset.) This is a way of standardizing the length of the datasets, such that each dataset contains the same amount of rows/dates. The desired start and end time is naturally chosen to be the first and last entry in the GED, i.e., January 1, 1989 to December 31, 2019. When data is resampled, with these entries in the datasets, missing dates and data are added to the datsets, completing the standardization.

---
<details>
    <summary>Click to see code!</summary>

```python
    def basic_handling_of_dates(self): 
        """Resamples dataframes using date columns in country_dfs according to scheme below.

        The method loops over all dataframes in country_dfs, and give each row a reference time. The 
        reference time is computed as date_start + (date_end - date_start)/2 corresponding to a datetime
        object. If the earliest reference time is unequal to the first date in df, then prepend an "empty" 
        row to the country specific dataframe. This "empty" row have date objects equal to first date
        in df, and zero in other entries. The same type of operations are conducted if the latest 
        reference time is unequal to the last date in df. Then an empty row is appended to the country 
        specific dataframe. At last, the country specific dataframe is resampled according to the date 
        of the reference time and date_start and date_end are removed. Resampling leads to filling in 
        empty rows between the first and last date in the country specific dataframe.    

        """
        tmp_ls = []
        cols_to_remove = ['type_of_violence', 'conflict_new_id', 'country_id', 'date_prec', 'type_of_conflict']
        for country in self.country_dfs:
            country = country.drop(columns=cols_to_remove) 
            country.date_start = pd.to_datetime(country.date_start, format='%Y-%m-%d')
            country.date_end = pd.to_datetime(country.date_end, format='%Y-%m-%d')
            country['ref_time'] = country.date_start + (country.date_end-country.date_start)/2
            if country.ref_time.min() != self.first_date:
                first_row = pd.Series([self.first_date, self.first_date, 0, 0, 0, 0, 0, 0, 0, self.first_date], 
                    index=['date_start', 'date_end', 'deaths_a', 'deaths_b', 'deaths_civilians', 
                           'deaths_unknown', 'best', 'high', 'low', 'ref_time'])
                country = country.append(first_row, ignore_index=True)
            if country.ref_time.max() != self.last_date:
                last_row =  pd.Series([self.last_date, self.last_date, 0, 0, 0, 0, 0, 0, 0, self.last_date], 
                    index=['date_start', 'date_end', 'deaths_a', 'deaths_b', 'deaths_civilians',
                           'deaths_unknown', 'best', 'high', 'low', 'ref_time'])
                country = country.append(last_row, ignore_index=True)
            country = country.set_index('ref_time')
            country = country.drop(columns=['date_start', 'date_end'])
            country = country.resample('D').sum()
            tmp_ls.append(country)
        self.country_dfs = tmp_ls

###############################################################################
        
data.basic_handling_of_dates()
```
</details>

---
**Ouput:**

| ref_time            |   deaths_a |   deaths_b |   deaths_civilians |   deaths_unknown |   best |   high |   low |
|:--------------------|-----------:|-----------:|-------------------:|-----------------:|-------:|-------:|------:|
| 1989-01-01 00:00:00 |          0 |          0 |                  0 |                0 |      0 |      0 |     0 |
| 1989-01-02 00:00:00 |          0 |          0 |                  0 |                0 |      0 |      0 |     0 |
| 1989-01-03 00:00:00 |          0 |          0 |                  0 |                0 |      0 |      0 |     0 |

| ref_time            |   deaths_a |   deaths_b |   deaths_civilians |   deaths_unknown |   best |   high |   low |
|:--------------------|-----------:|-----------:|-------------------:|-----------------:|-------:|-------:|------:|
| 1989-01-01 00:00:00 |          0 |          0 |                  0 |                0 |      0 |      0 |     0 |
| 1989-01-02 00:00:00 |          0 |          0 |                  0 |                0 |      0 |      0 |     0 |
| 1989-01-03 00:00:00 |          0 |          0 |                  0 |                0 |      0 |      0 |     0 |

### Step 10: Aggregate data by year and week
In this step all rows from the individual dataset are grouped into years and weeks according to the ISO week date system based on `ref_time`. The ISO week date system means that every year has either 52 or 53 full weeks, with the assumption of a 7-day week starting on Mondays. As a consequence of this system, two examples are given; 29th-31st of December 2014 belongs to week 1 of 2015 and 1st-3rd of January 2016 belongs to week 53 of 2015. These examples put together gives the most "extreme" case, in the sense that the number of dates belonging to other years than the calender year 2015 is the highest, when these dates are considered to be a part of 2015 in the ISO week date system. More generally, the dates from 4th of January to 28th of December in any calender year are considered to be a part of the same year in the ISO week date system.       

Furthermore, the `best` column is summed within each group, and is labeled `battle_deaths`. All of the following columns are dropped in this step
```
'deaths_a', 'deaths_b', 'deaths_civilians', 'deaths_unknown', 'low', 'high'
```

Since we are operating with the ISO week date system, the first and last row in each individual dataset have indexes corresponding to week 52 in year 1988 and week 1 in year 2020, respectively. These indexes results from the dates 1st of January 1989 and 30th-31th of December 2019. Since there are very few dates accounting for a whole week, we drop these rows.


---
<details>
    <summary>Click to see code!</summary>

```python
    def aggregate_by_year_and_week(self):
        """Aggregates the best column for every dataframe in country_dfs to levels year and week.

        The method uses the reference time to aggregate each best column for every dataframe in country_dfs
        to main level year and sub-level week. The year-week level follows the ISO week date system, meaning that 
        there is either 52 or 53 full weeks in a year and that every week starts on a Monday. 
        This operation involves a full representation of each year, meaning that if for example best=0 for all
        dates in a year, every week in that year will have entries 0. When aggregation has been executed the 
        column best is named battle_deaths. All other columns, except battle_deaths, year and week are dropped
        in this method. Additionally, when following the ISO week date system, the 1st of January 1989 is considered 
        to belong to week 52 in 1988, and 30th-31th December of 2019 belongs to week 1 in 2020. Since these two weeks 
        only considers data from a few dates, we drop these two weeks from our country specific data frames. 

        """
        tmp_ls = []
        for country in self.country_dfs:
            country['year'] = country.index.isocalendar().year
            country['week'] = country.index.isocalendar().week 
            country = country.groupby(['year', 'week'])['best'].agg(['sum'])
            country.columns = ['battle_deaths'] 

            country = country.iloc[1:-1] # Drop week 52 in 1988 and week 1 of 2020

            tmp_ls.append(country)
        self.country_dfs = tmp_ls

###########################################################################

data.aggregate_by_year_and_week()
```
</details>

---
**Output:**

|           |   battle_deaths |
|:----------|----------------:|
| (1989, 1) |               0 |
| (1989, 2) |               4 |
| (1989, 3) |               0 |

|           |   battle_deaths |
|:----------|----------------:|
| (1989, 1) |               0 |
| (1989, 2) |               0 |
| (1989, 3) |               0 |

### Step 11: Introducing v2x_polyarchy to the datasets
With the **Country-Year: V-Dem Core** dataset, we introduce the `v2x_polyarchy` [(V-Dem Codebook v10)][3] covariate into the two datasets. The covariate is an index representing how close to an ideal electoral democracy a state/country is in a given year. The index is defined in the interval [0,1], where an index very close to 1 represents a state being very close to an ideal electoral democracy.
The V-Dem dataset have two columns called `year` and `country_name`, which are used to get the right values of `v2x_polyarchy` into the datasets. Since the data were first split by Colombia and then by the DR Congo, the program requires the user to follow this particular order when giving values of `country_name`.

It's also important to point out that when `v2x_polyarchy` have been introduced into the datasets, say 0.154 for the DR Congo in 1989, it actually represent the `v2x_polyarchy` in 1988. However, this can easily be changed in the code by setting *time_shift* to 0. 
[//]: # (When giving the countries' names in the program, the user must open the the V-Dem dataset to see what the different countries are called. In our case, Colombia is called *Colombia* and the DR Congo is called *Democratic Republic of the Congo*. The ordering of these two country names are important, and must follow the ordering as earlier. First, the name of Colombia and then the DR Congo. The user may also choose if `v2x_polyarchy` should be time-shifted. If *time_shift* is -1, then covariate `v2x_polyarchy` in our two datasets represents th)


---
<details>
    <summary>Click to see code!</summary>

```python
    def add_v2x_polyarchy(self, vdem_data_path, vdem_country_names, time_shift=0):
        """Adds column v2x_polyarchy from the V-DEM data (inputted path) into dataframes of country_dfs.

        The method reads the V-DEM data from vdem_data_path, and inserts the column v2x_polyarchy to each
        dataframe in country_dfs using the common column year. vdem_country_names is used to get data from
        the right country in the V-Dem dataset into the country specific dataframe. vdem_country_names must 
        represent the same countries as country_ls from select_countries(), and strictly in that same order.
        If time_shift is given, and is -1 for example, the v2x_polyarchy at year 2009 in the resulting 
        dataframes of country_dfs from this method, will actually correspond to the v2x_polyarchy recorded for 
        the year 2008.    

        Args: 
           vdem_data_path (str): Absolute or relative path to V-DEM data.
           vdem_country_names (list): A list of strings of the country_name in V-Dem dataset. 
           time_shift (int): An integer representing the time-shift in v2x_polyarchy.

        """
        tmp_ls = []
        vdem = pd.read_csv(vdem_data_path)
        for country, vdem_country_name in zip(self.country_dfs, vdem_country_names):
            vdem_country = vdem[(vdem.country_name == vdem_country_name)]
            year_v2x_polyarchy_dict = dict(zip(vdem_country.year, vdem_country.v2x_polyarchy))

            country['year_ref'] = country.index.get_level_values('year') + time_shift
            country['v2x_polyarchy'] = country['year_ref'].map(year_v2x_polyarchy_dict)

            country = country.drop(columns=['year_ref']) 
            tmp_ls.append(country)
        self.country_dfs = tmp_ls

###############################################################################

data.add_v2x_polyarchy(vdem_data_path="./raw_data/V-Dem-CY-Core-v10.csv", 
                      vdem_country_names=["Colombia", "Democratic Republic of the Congo"], 
                      time_shift=-1)
```

</details>

---
**Output:**

|           |   battle_deaths |   v2x_polyarchy |
|:----------|----------------:|----------------:|
| (1989, 1) |               0 |           0.432 |
| (1989, 2) |               4 |           0.432 |
| (1989, 3) |               0 |           0.432 |

|           |   battle_deaths |   v2x_polyarchy |
|:----------|----------------:|----------------:|
| (1989, 1) |               0 |           0.154 |
| (1989, 2) |               0 |           0.154 |
| (1989, 3) |               0 |           0.154 |

### Step 12: Introducing World Bank indicators to the datasets
We can obtain data from the World Bank by using it's **API** (**A**pplication **P**rogramming **I**nterface) - which is compatible with Python through the package `wbdata`. This requires an internet connection. Interesting to us are some specific indicators, namely `NY.GDP.PCAP.KD` and `SP.POP.TOTL`. Respectively, they represent the *GDP per capita (in constant 2010 US$)* and *Total population* of a country in a given year. In our datasets, these indicators will be called `gdp_pc` and `pop_tot`. We extract these data with the API by defining the countries of interest (3 letter abbreviation, COL - Colombia, COD - DR Congo), a time interval and, of course, the indicators of interest. The indicators are introduced to the correct datasets at the correct rows using the ordering of countries' names and years. Like `v2x_polyarchy`, we apply a shift in time to these covariates when represented in the datasets. For example; the `gdp_pc` in 1989 for the dataset of Colombia actually represents the value recorded in 1988.

---
<details>
    <summary>Click to see code!</summary>

```python
    def add_world_bank_indicators(self, wb_country_names, indicators_dict, time_shift=0):
        """Adds columns of indicator data from the World Bank to every dataframe in country_dfs.

        The method adds World Bank indicators as columns in every dataframe in country_dfs using mutual year values.
        wb_country_names must represent the same countries as in country_ls from select_countries(), and strictly
        in that order. This list (wb_country_names) is used to get indicator values from the right country to the
        right dataframe. The key value in indicators_dict is the name of the index, while the value is a more descriptive
        name and will correspond to the column name in each dataframe.         
        If time_shift is given, and is -1 for example, an unspecified World Bank indicator at year 2009 in the resulting 
        dataframes of country_dfs from this method, will actually correspond to the unspecified World Bank indicator
        recorded for the year 2008.   

        Args:
           wb_country_names (list): A list of strings representing the abbreviated countries in the World Bank.
           indicators_dict (dict): A dictionary of keys corresponding to World Bank indicators, and more
            descriptive names of the indicators as the values.
           time_shift (int): An integer representing the time-shift in World Bank indicators.

        """
        tmp_ls = []
        for country, wb_country_name in zip(self.country_dfs, wb_country_names):
            country['year_ref'] = country.index.get_level_values('year') + time_shift

            data_date = datetime.datetime(country.year_ref.min(), 1, 1), datetime.datetime(country.year_ref.max(), 1, 1)
            wb_df = wbdata.get_dataframe(indicators_dict, country=wb_country_name, data_date=data_date)
            wb_df.index = wb_df.index.rename("year")
            wb_df.index = wb_df.index.astype(int)

            for indicator in wb_df.columns:
                year_indicator_dict = dict(zip(wb_df.index, wb_df[indicator]))
                country[indicator] = country["year_ref"].map(year_indicator_dict)

            country = country.drop(columns=["year_ref"])
            tmp_ls.append(country)
        self.country_dfs = tmp_ls

###############################################################################

wb_country_names = ["COL", "COD"]
indicators_dict = {"NY.GDP.PCAP.KD": "gdp_pc",
                   "SP.POP.TOTL": "pop_tot"} 
data.add_world_bank_indicators(wb_country_names=wb_country_names,
                               indicators_dict=indicators_dict,
                               time_shift=-1)
```

</details>

---

**Output:**

|           |   battle_deaths |   v2x_polyarchy |   gdp_pc |     pop_tot |
|:----------|----------------:|----------------:|---------:|------------:|
| (1989, 1) |               0 |           0.432 |  4309.16 | 3.18225e+07 |
| (1989, 2) |               4 |           0.432 |  4309.16 | 3.18225e+07 |
| (1989, 3) |               0 |           0.432 |  4309.16 | 3.18225e+07 |

|           |   battle_deaths |   v2x_polyarchy |   gdp_pc |     pop_tot |
|:----------|----------------:|----------------:|---------:|------------:|
| (1989, 1) |               0 |           0.154 |  811.999 | 3.24438e+07 |
| (1989, 2) |               0 |           0.154 |  811.999 | 3.24438e+07 |
| (1989, 3) |               0 |           0.154 |  811.999 | 3.24438e+07 |

### Step 13: Write data to .csv
The datasets of Colombia and DR Congo have been completed from Step 12, and is therefore written to a portable and well-known format, csv-files. Again, when providing output names for the csv-files, the user must be aware of the ordering of initial split as earlier mentioned. 
 
---
<details>
    <summary>Click to see code!</summary>

```python
    def write_countries_to_csv(self, output_names):
        """Writes each dataframe in country_dfs to csv-file with file names as entries in inputted list output_names.

        The method writes each dataframe in country_dfs to a csv-file. The names of each file is specified in 
        inputted list output_names, and must be strictly ordered as country_ls from select_countries(). 

        Args: 
           output_names (list): A list of strings of output names. 

        """
        for country, output_name in zip(self.country_dfs, output_names):
            country.to_csv('{}.csv'.format(output_name))   

###############################################################################

data.write_countries_to_csv(output_names=["colombia_example", "congo_example"]) 
```

</details>

---

**Output:**
*example_colombia.csv and example_congo.csv located in root/data folder.*


## References

**References in text:** 

- [1]: Högbladh Stina, 2020, “UCDP GED Codebook version 20.1”, Department of Peace and Conflict Research, Uppsala University
- [2]: Pettersson, Therese (2020) UCDP/PRIO Armed Conflict Dataset Codebook v 20.1
- [3]: Coppedge, Michael, et al. "V-Dem Quickstart Guide v9." (2019).


[1]: https://ucdp.uu.se/downloads/ged/ged201.pdf
[2]: https://ucdp.uu.se/downloads/ucdpprio/ucdp-prio-acd-201.pdf
[3]: https://www.v-dem.net/files/59/V-Dem%20Startquide.pdf

**References to datasets:**

- Sundberg, Ralph, and Erik Melander, 2013, “Introducing the UCDP Georeferenced
Event Dataset”, Journal of Peace Research, vol.50, no.4, 523-532
- Gleditsch, Nils Petter; Peter Wallensteen, Mikael Eriksson, Margareta Sollenberg & Håvard Strand (2002) Armed Conflict 1946–2001: A New Dataset. Journal of Peace Research 39(5): 615–637
- Pettersson, Therese & Magnus Öberg (2020). Organized violence, 1989-2019. Journal of Peace Research 57(4).
- Coppedge, Michael, John Gerring, Carl Henrik Knutsen, Staffan I. Lindberg, Jan Teorell, David Altman, Michael Bernhard, M. Steven Fish, Adam Glynn, Allen Hicken, Anna Luhrmann, Kyle L. Marquardt, Kelly McMann, Pamela Paxton, Daniel Pemstein, Brigitte Seim, Rachel Sigman, Svend-Erik Skaaning, Jeffrey Staton, Steven Wilson, Agnes Cornell, Nazifa Alizada, Lisa Gastaldi, Haakon Gjerløw, Garry Hindle, Nina Ilchenko, Laura Maxwell, Valeriya Mechkova, Juraj Medzihorsky, Johannes von Römer, Aksel Sundström, Eitan Tzelgov, Yi-ting Wang, Tore Wig, and Daniel Ziblatt. 2020. ”V-Dem Country–Year Dataset v10”. Varieties of Democracy (V-Dem) Project. https://doi.org/10.23696/vdemds20.
- Pemstein, Daniel, Kyle L. Marquardt, Eitan Tzelgov, Yi-ting Wang, Juraj Medzihorsky, Joshua Krusell, Farhad Miri, and Johannes von Römer. 2020. “The V-Dem Measurement Model: Latent Variable Analysis for Cross-National and Cross-Temporal Expert-Coded Data”. V-Dem Working Paper No. 21. 5th edition. University of Gothenburg: Varieties of Democracy Institute.

