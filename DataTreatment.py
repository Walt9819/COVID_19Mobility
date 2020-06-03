import pandas as pd
#Hola
path = "D:\\Documentos\\MT\\Mobility\\MobilityGitHub\\Data\\"

conf_data = pd.read_csv(path + "Confirmados.csv")
def_data = pd.read_csv(path + "Defunciones.csv")
neg_data = pd.read_csv(path +"Negativos.csv")
sos_data = pd.read_csv(path + "Sospechosos.csv")
mob_data = pd.read_csv(path + "GlobalMobilityReport.csv", sep=',', error_bad_lines=False, index_col=False, dtype='unicode')

mob_data = mob_data.fillna("-")
mob_data = mob_data.loc[mob_data['country_region_code'] == "MX"]
mob_data = mob_data.drop(["sub_region_2", "country_region_code", "country_region"], axis=1)
mob_data.columns = ["Region", "Date", "RetailRecreation", "GroceryPharmacy", "Parks", "TransitStations", "Workplaces", "Residential"]
mob_data = mob_data.replace(mob_data["Region"].unique(), ['NACIONAL', 'AGUASCALIENTES', 'BAJA CALIFORNIA',
'BAJA CALIFORNIA SUR', 'CAMPECHE', 'CHIAPAS', 'CHIHUAHUA', 'COAHUILA', 'COLIMA', 'DURANGO',
'GUANAJUATO', 'GUERRERO', 'HIDALGO', 'JALISCO', 'DISTRITO FEDERAL', 'MICHOACAN',
'MORELOS', 'NAYARIT', 'NUEVO LEON', 'OAXACA', 'PUEBLA', 'QUERETARO', 'QUINTANA ROO',
'SAN LUIS POTOSI', 'SINALOA', 'SONORA', 'MEXICO', 'TABASCO', 'TAMAULIPAS', 'TLAXCALA',
'VERACRUZ', 'YUCATAN', 'ZACATECAS'])

df = pd.DataFrame()
states = neg_data["nombre"]
for date in neg_data.columns[3:]:
    tdf = pd.DataFrame()
    for i, negv in enumerate(neg_data[date]):
        if date in conf_data.columns:
            confv = conf_data[date][i]
        else:
            confv = 0
        if date in def_data.columns:
            defv = def_data[date][i]
        else:
            defv = 0
        if date in sos_data.columns:
            sosv = sos_data[date][i]
        else:
            sosv = 0
        tdf = pd.concat([tdf, pd.DataFrame([pd.Series([states[i], date, confv, sosv, negv, defv])])], ignore_index = True)
    df = pd.concat([df, tdf])
df.columns = ["Region", "Date", "Cases", "Suspicious", "Negative", "Deaths"]
df["Date"] = pd.to_datetime(df["Date"], format = "%d-%m-%Y")
#df = df.loc[df["Date"].isin(mob_data["Date"].unique())]
df = df.replace("Nacional", "NACIONAL")
df = df.sort_values(by=["Region", "Date"])
mob_data["Date"] = pd.to_datetime(mob_data["Date"], format = "%Y-%m-%d")
mob_data = mob_data.sort_values(by=["Region", "Date"])
allData = pd.merge(df, mob_data, on=["Region", "Date"])

allData.to_csv(path + "DataMexico.csv", index=False)
