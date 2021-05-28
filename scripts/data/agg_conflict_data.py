# Load necessary packages
import os
import pandas as pd
import wbdata
import datetime
import numpy as np

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

    def drop_columns(self, col_ls):
        """Drops/removes columns in df specified in inputted list.

        Args: 
           col_ls (list): A list of str's representing columns in df.

        """
        self.df = self.df.drop(columns=col_ls)    

    def remove_date_prec(self, precision_ls):
        """Removes rows in df with date_prec equal to entries in inputted list.

        Args:
           precision_ls (list): A list of integers representing date_prec levels.

        """
        for precision in precision_ls:
            self.df = self.df.drop(self.df[self.df.date_prec == precision].index)  

    def remove_type_of_violence(self, type_of_violence_ls):
        """Removes rows in df with type_of_violence equal to entries in inputted list.

        Args:
           type_of_violence_ls (list): A list of integers representing type_of_violence levels.

        """        
        for violence_type in type_of_violence_ls: 
            self.df = self.df.drop(self.df[self.df.type_of_violence == violence_type].index)

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

    def remove_type_of_conflict(self, type_of_conflict_ls):
        """Removes rows in df with type_of_conflict equal to entries in inputted list.

        Args: 
           type_of_conflict_ls (list): A list of integers representing type_of_conflict levels.

        """  
        for conflict_type in type_of_conflict_ls:
            self.df = self.df.drop(self.df[self.df.type_of_conflict == conflict_type].index)   

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

    def write_countries_to_csv(self, output_names):
        """Writes each dataframe in country_dfs to csv-file with file names as entries in inputted list output_names.

        The method writes each dataframe in country_dfs to a csv-file. The names of each file is specified in 
        inputted list output_names, and must be strictly ordered as country_ls from select_countries(). 

        Args: 
           output_names (list): A list of strings of output names. 

        """
        for country, output_name in zip(self.country_dfs, output_names):
            country.to_csv('{}.csv'.format(output_name))        