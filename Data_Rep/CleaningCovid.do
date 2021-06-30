/* ------------------------------------------------------------------
                   Universidad de San Andrés
				Pronósticos Financieros - Examen final
	         Armado de base de datos - Datos COVID-19
				  Kleiman, Pacheco y Riquelme
 ------------------------------------------------------------------ */

* In this do file we are goint go manipulate the COVID-19 data.
* We will download the data from internet. 
* If you want to replicate this, take into account that the downloading and decompressing process will take A LOT of time.

* First, we are going to deal with COVID-19 cases and deaths. The data will be downloaded from the Ministerio de Salud.

copy "https://sisa.msal.gov.ar/datos/descargas/covid-19/files/Covid19Casos.zip" covid19_cases.zip

unzipfile covid19_cases.zip, replace

import delimited "API_SP.DYN.LE00.IN_DS2_es_csv_v2_2167433.csv",  varnames(4) encoding(UTF-8) clear

* We keep our variables of interest.

keep sexo edad residencia_provincia_nombre cuidado_intensivo fallecido clasificacion clasificacion_resumen fecha_apertura 

* Confirmed cases.

keep if clasificacion_resumen == "Confirmado"
drop clasificacion*

* A variable is generated to count the cases once we collapse de data.

gen caso = 1

* We generate some variable to have other potentially useful stats.

gen muerto = (fallecido == "SI")
gen terapia = (cuidado_intensivo == "SI")

gen sex = 1 if sexo == "F"
replace sex = 0 if sexo == "M"
replace sex = t if sexo == "NR"

drop cuidado_intensivo fallecido sexo

* Province variable.

rename resid provincia
encode provincia, gen(prov)
drop provincia
rename fecha_apertura fecha

order provincia prov fecha caso edad sex muerto terapia

* We finally collapse the data.

collapse (sum) caso muerto terapia (mean) edad sex, by(fecha prov)
sort prov

* What we have now is the data in a panel format. This is, by province and day.

* We collapse the variables again to have our time series.

collapse (sum) caso muerto terapia (mean) edad sex, by(fecha)

* Finally, we export.

save Covid_TimeSeries.dta


*******************************************************************************

* In this section, we're going to deal with the applied vaccinations in the territory.

copy "https://sisa.msal.gov.ar/datos/descargas/covid-19/files/datos_nomivac_covid19.zip" vaccines.zip

unzipfile covid19_cases.zip, replace

import delimited "",  varnames(4) encoding(UTF-8) clear

* Variables of interest.

keep fecha_aplicacion vacuna orden_dosis

* We generate this variable so we can count the applied dozes per day.

gen vacuna = 1

* We collapse the data.

collapse (sum) vacuna, by(fecha)
drop if fecha_aplicacion == "S.I."
rename fecha_aplicacion fecha

* This is a time series. We export it

save vacunas.dta

* Last but not least, we merge all the downloaded data.

use Covid_TimeSeries.dta

merge 1:1 fecha using vacunas.dta
drop if fecha == "2021-06-09"
drop _merge

* We export our final .dta file.

save CovidAndVaccines.dta

* Time series format.

replace vacuna
replace vacuna = 0 if vacuna == .
gen time = daily(fecha, "YMD")
format time %td
tsset time
tsline caso


