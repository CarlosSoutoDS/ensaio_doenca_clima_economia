#**Economic Impact of Climatic Factors on the Health Sector in Brazil - Scientific Essay**
Carlos Souto dos Santos Filho∗ (author)
∗Statistics Student – UNESP, carlos.souto@unesp.br CV: lattes.cnpq.br/1205287661151474 

##**1. Introduction**
For a long time, the importance of greenhouse gas emissions related to food production, energy systems, and global economic impact was neglected. Interest in the subject gained prominence in 2006 with the publication of the "The Stern Review on the Economics of Climate Change" by Nicholas Stern. This review detailed the economic impacts of climate change and emphasized that mitigating greenhouse gas emissions and adaptation were economically viable strategies for the future of humanity. The author highlighted the influence of climate change on society, comparing it to the damages of two perennial world wars in the 20th century.
Interest in the subject has grown exponentially, with publications focusing on quantifying and monetizing the effects of climatic variables on agriculture, forestry, water resources, coastal zones, energy consumption, air quality, and human health.
Another growing area of investigation is the economic impact associated with climate on health. A classic meta-analysis study associated average temperatures with mortality rates from respiratory and cardiovascular diseases. Other researchers evaluated the influence of climate on other infectious diseases using statistical models. Patz, Campbell-Lendrum, Holloway, Foley (2005) conducted predictive studies on health risks under future climate change projections, indicating increases in morbidity and mortality on a global scale.
This essay aimed to estimate cases of cardiovascular disease in a Brazilian region based on climatic factors using a linear regression model, assess treatment costs, forecast national population growth, and explore future climate change scenarios.

##**2. Methodology**

###2.1 Information Sources
Information sources are publicly accessible and categorized by sector:
_▪ Public Health:_
>> Department of Informatics of the Unified Health System (DATASUS), available at: https://datasus.saude.gov.br/informacoes-de-saude-tabnet/
_▪ Climate:_
>> Meteorological Database of the National Institute of Meteorology (INMET), available at: https://bdmep.inmet.gov.br/
>> CLIMBra (CABra), available at: https://hess.copernicus.org/articles/25/3105/2021/ & https://doi.org/10.57760/sciencedb.02316
_▪ Average Annual Growth Rate_
>> Wittgenstein Center for Demography and Global Human Capital (2018). Wittgenstein Center Data Explorer, available at:
https://dataexplorer.wittgensteincentre.org/wcde-v2/
_▪ Brazilian Deprivation Index (IBP)_
>> Cidacs, available at:
https://docs.google.com/forms/d/e/1FAIpQLScG7fffQVuZE_yCSweeQ06iNr2jZx7AG4kuX8iziPY7Gee18w/viewform

##2.2 Data

###2.2.1 Historical Data
Files in .csv format were obtained from the DATASUS database on:
▪ Hospital Morbidity - by place of hospitalization
>> Admissions by Year/month of processing and Federation Unit:
>> Period: 2010-2019
>> Pathologies:
- Chapter ICD-10: V. Mental and behavioral disorders
- Chapter (ICD-10M) Musculoskeletal system and technical diseases. conjunctive
- Chapter ICD-10: II. Neoplasms (tumors)
- Chapter ICD-10: X. Diseases of the respiratory system
- Categories Causes: Traffic accident
▪ Mortality
>> Deaths by Residence by Year / month of Death and Federation Unit
- Group: Ischemic heart diseases, Cerebrovascular diseases (CVD)
- Traffic Accident Category
▪ DENGUE (Notification)
>> Probable Cases by Month Notification and UF of notification
>> Period: 2010-2019
▪ Hospital Cost
>> Total value per Year/month processing and Federation Unit
- Chapter ICD-10: XIII. Musculoskeletal system and connective tissue diseases
- Chapter ICD-10: V. Mental and behavioral disorders
- Chapter ICD-10: II. Neoplasms (tumors)
From the BDMEP_INMET database, files in .csv format were obtained about:
▪ Climatic Factors
>> Measurement Frequency: Monthly
>> Station Code
>> Status: Operating
- Total sunshine, monthly(h)
- Total precipitation, monthly (mm)
- Average maximum temperature, monthly (°C);
- Compensated average temperature, monthly (°C)
From the “Wittgenstein Center Data Explorer” database, files in .csv format were obtained about:
▪ Average Annual Growth Rate
- Scenario (SSP1, SSP2, SSP3)
- "Area"
- "Period"(1950-1955 ... 2095-2100.)
- "Rate"
▪ Brazilian Deprivation Index
- big_region_name,
- measure_1f_12
  
###2.2.2 Forecasts (2020-2100)
From the CLIMBra database
>> “Catchment data”
- CABRa_id (streamflow gauge)
- Scenario SSP2 | 4.5
- Maximum Temperature
  
##2.3 Statistical Analysis
Exploratory Data Analysis (EDA) was conducted, followed by a simple and multiple linear regression model following the Gauss-Markov paradigms and the "LINE" protocol. More details on the procedures can be found on the GitHub profile. The results were obtained using the R software.
