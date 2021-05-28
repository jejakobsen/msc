from agg_conflict_data import AggregateConflictData

import os
os.chdir('../../') # Go to root directory
os.chdir('./data/') # Go to data directory


data = AggregateConflictData(ged_data_path='./raw_data/ged201.csv') 
print(data.df.head(3).to_markdown())

# Columns to drop 
col_to_drop = ['id', 'year', 'active_year', 'relid', 'code_status', 'conflict_dset_id',
               'dyad_dset_id', 'side_a_dset_id', 'side_b_dset_id', 'where_description',
               'conflict_name', 'dyad_new_id', 'dyad_name', 'side_a_new_id', 'gwnoa',
               'side_a', 'side_b_new_id', 'gwnob', 'side_b', 'number_of_sources',
               'source_article', 'source_office', 'source_date', 'source_headline',
               'source_original', 'where_prec', 'where_coordinates', 'adm_1', 'adm_2',
               'latitude', 'longitude', 'geom_wkt', 'priogrid_gid', 'country',
               'region', 'event_clarity']

data.drop_columns(col_ls=col_to_drop) 
print(data.df.head(3).to_markdown())

# Remove non-state conflict and one-sided violence
data.remove_type_of_violence(type_of_violence_ls=[2, 3]) 
print(data.df.head(3).to_markdown())

# Drop rows with date_prec = 4 or 5
data.remove_date_prec(precision_ls=[4, 5]) 
print(data.df.head(3).to_markdown())

data.map_type_of_conflict_to_conflict_id(acd_data_path="./raw_data/ucdp-prio-acd-201.csv")
print(data.df.head(3).to_markdown())

# Remove conflicts of type extrasystemic and interstate
data.remove_type_of_conflict(type_of_conflict_ls=[1, 2])
print(data.df.head(3).to_markdown())

# Colombia, Congo
data.select_countries(country_ls=[100, 490])
print(data.country_dfs[0].head(3).to_markdown())
print(data.country_dfs[1].head(3).to_markdown())

data.add_low_and_high_to_best_if_best_is_zero()
print(data.country_dfs[0].head(3).to_markdown())
print(data.country_dfs[1].head(3).to_markdown())

data.basic_handling_of_dates()
print(data.country_dfs[0].head(3).to_markdown())
print(data.country_dfs[1].head(3).to_markdown())

data.aggregate_by_year_and_week()
print(data.country_dfs[0].head(3).to_markdown())
print(data.country_dfs[1].head(3).to_markdown())

data.add_v2x_polyarchy(vdem_data_path="./raw_data/V-Dem-CY-Core-v10.csv", 
                      vdem_country_names=["Colombia", "Democratic Republic of the Congo"], 
                      time_shift=-1)
print(data.country_dfs[0].head(3).to_markdown())
print(data.country_dfs[1].head(3).to_markdown())


wb_country_names = ["COL", "COD"]
indicators_dict = {"NY.GDP.PCAP.KD": "gdp_pc",
                   "SP.POP.TOTL": "pop_tot"} 
data.add_world_bank_indicators(wb_country_names=wb_country_names,
                               indicators_dict=indicators_dict,
                               time_shift=-1)
print(data.country_dfs[0].head(3).to_markdown())
print(data.country_dfs[1].head(3).to_markdown())

data.write_countries_to_csv(output_names=["example_colombia", "example_congo"]) 