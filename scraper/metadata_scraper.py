import requests
import regex as re
import pandas as pd

URL = 'https://github.com/covid19-forecast-hub-europe/covid19-forecast-hub-europe/tree/main/data-processed'

models = ['epiforecasts-EpiExpert','epiforecasts-EpiNow2','EuroCOVIDhub-baseline','EuroCOVIDhub-ensemble','IEM_Health-CovidProject','ILM-EKF','LANL-GrowthRate','USC-SIkJalpha','RobertWalraven-ESG','DSMPG-bayes','UMass-SemiMech','UMass-MechBayes','bisop-seirfilterlite','epiforecasts-EpiExpert_Rt','epiforecasts-EpiExpert_direct','MUNI-ARIMA','Karlen-pypm','MUNI-VAR','MUNI-LaggedRegARIMA','epiforecasts-weeklygrowth','prolix-euclidean','MUNI_DMS-SEIAR','bisop-seirfilter','FIAS_FZJ-Epi1Ger' ,'itwm-dSEIR','ITWW-county_repro','MIT_CovidAnalytics-DELPHI','UNIPV-BayesINGARCHX','LeipzigIMISE-SECIR','HZI-AgeExtendedSEIR','CovidMetrics-epiBATS','KITmetricslab-bivar_branching','UB-BSLCoV','ICM-agentModel','MIMUW-StochSEIR','MOCOS-agent1','PL_GRedlarski-DistrictsSum','ULZF-SEIRC19SI'] 

columns = ['team_name', 'model_name', 'model_abbr', 'model_contributors', 'methods', 'data_inputs', 'methods_long']
#processed/CovidMetrics-epiBATS/metadata-CovidMetrics-epiBATS.txt
metadata = []

for model in models:
	res = []
	#navigate to URL 
	dest = URL + '/' + model + '/metadata-' + model + '.txt'
	page = requests.get(dest)

	meta_raw = re.sub(r'[^\x00-\x7F]+',' ', page.text)
	#print(meta_raw)

	for col in columns: 
		#extract info (use regex)
		env = col + ': ' + '(.*)' + '</td>'

		match = re.search(env, meta_raw)

		if match is not None:
			match = match.group(1)
		else:
			match = ''

		emails = '&lt(.*?)&gt;'
		match = re.sub(emails, '', match)

		otherstuff = '&quot;|&gt;|&#39;'
		match = re.sub(otherstuff, '', match)

		res.append(match)
	
	metadata.append(res)


#convert to dataframe
metadata_df = pd.DataFrame(metadata, columns = columns)
metadata_df.to_csv("metadata.csv", index = False)

#manually add model_type column