from agg_conflict_data import AggregateConflictData

import os
os.chdir('../../') # Go to root directory
os.chdir('./data/') # Go to data directory


data = AggregateConflictData(ged_data_path='./raw_data/ged201.csv') 

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

# Remove non-state conflict and one-sided violence
data.remove_type_of_violence(type_of_violence_ls=[2,3]) 

# Drop rows with date_prec = 4 or 5
data.remove_date_prec(precision_ls=[4,5]) 

data.map_type_of_conflict_to_conflict_id(acd_data_path="./raw_data/ucdp-prio-acd-201.csv")

# Remove conflicts of type extrasystemic and interstate
data.remove_type_of_conflict(type_of_conflict_ls=[1, 2])

# Colombia, Congo, Ethiopia, Mali, Pakistan, Burma, Iraq, Nigeria, Uganda, Sierra Leone
data.select_countries(country_ls=[100, 490,
                                  530, 432,
                                  770, 775,
                                  645, 475,
                                  500, 451])

data.add_low_and_high_to_best_if_best_is_zero()

data.basic_handling_of_dates()

data.aggregate_by_year_and_week()

data.add_v2x_polyarchy(vdem_data_path="./raw_data/V-Dem-CY-Core-v10.csv", 
                      vdem_country_names=["Colombia", "Democratic Republic of the Congo",
                                          "Ethiopia", "Mali",
                                          "Pakistan", "Burma/Myanmar",
                                          "Iraq", "Nigeria",
                                          "Uganda", "Sierra Leone"], 
                      time_shift=-1)


wb_country_names = ["COL", "COD",
                    "ETH", "MLI",
                    "PAK", "MMR",
                    "IRQ", "NGA",
                    "UGA", "SLE"]
indicators_dict = {"NY.GDP.PCAP.KD": "gdp_pc",
                   "SP.POP.TOTL": "pop_tot"} 
data.add_world_bank_indicators(wb_country_names=wb_country_names,
                               indicators_dict=indicators_dict,
                               time_shift=-1)

data.write_countries_to_csv(output_names=["colombia", "congo", 
                                          "ethiopia", "mali",
                                          "pakistan", "myanmar",
                                          "iraq", "nigeria",
                                          "uganda", "sleone"]) 