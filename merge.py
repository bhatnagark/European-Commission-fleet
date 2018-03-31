import pandas as pd
eco_df = pd.read_csv('economic.csv')
eco_df.rename(columns={'Vessel  ID': 'vid'}, inplace=True)
print(eco_df)
fish_df = pd.read_csv('fish2.csv')
# print(fish_df)

# result = pd.concat([eco_df, fish_df], axis=1, join_axes=[eco_df.index])
# result = pd.DataFrame()
# result = pd.concat([eco_df, fish_df], axis=1, join='inner')
# print(result)

result = pd.merge(eco_df, fish_df, on=['vid', 'Year'])
print(result)

dup = eco_df.duplicated(['vid', 'Year']).tolist()
print("number of duplicates = ", sum(dup))

true_index_list = []
for i, b in enumerate(dup):
    if b is True:
        true_index_list.append(i)

print("row numbers of duplicates = ", true_index_list)

print(result.duplicated(['vid', 'Year']))


result.to_csv('merged_data.csv')
