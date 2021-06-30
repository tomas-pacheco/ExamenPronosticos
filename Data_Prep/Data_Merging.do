/* ------------------------------------------------------------------
                   Universidad de San Andrés
				Pronósticos Financieros - Examen final
	                 Armado de base de datos
				  Kleiman, Pacheco y Riquelme
 ------------------------------------------------------------------ */

 * En este do file presentamos el armado de la base de datos.

global main "G:\Mi unidad\UdeSA\Pronósticos\Final\Data"

global dep = "$main\dep"

global dep = "G:\Mi unidad\UdeSA\Pronósticos\Final\Data\dep\Nueva carpeta"

cd "$dep"

* Importamos datos de las reservas del BCRA.

import excel "Reservas_BC.xlsx", firstrow clear
replace Fecha = "12/2/2019" in 1

split Fecha, p("/")

foreach var of varlist Fecha1 Fecha2{
replace `var' = "01" if `var' == "1"
replace `var' = "02" if `var' == "2"
replace `var' = "03" if `var' == "3"
replace `var' = "04" if `var' == "4"
replace `var' = "05" if `var' == "5"
replace `var' = "06" if `var' == "6"
replace `var' = "07" if `var' == "7"
replace `var' = "08" if `var' == "8"
replace `var' = "09" if `var' == "9"
}

gen time = Fecha3 + "-" + Fecha1 + "-" + Fecha2
drop Fecha* 

save TEMP_Reservas.dta

* Emprolijamos la tasa de interés mensual.

import delimited "Tasa_Interes_Mensual.csv", clear

rename v2 time
rename v3 tasa

split time, p("/")
gen timeb = time3 + "-" + time2 + "-" + time1
drop time1 time2 time3
drop time 
rename timeb time

save TEMP_TasaInt.dta

* Emprolijamos Base monetaria

import excel "Base_Monetaria.xlsx" ,firstrow clear
drop C

replace fecha = "12/2/2019" in 1
split fecha, p("/")

foreach var of varlist fecha1 fecha2{
replace `var' = "01" if `var' == "1"
replace `var' = "02" if `var' == "2"
replace `var' = "03" if `var' == "3"
replace `var' = "04" if `var' == "4"
replace `var' = "05" if `var' == "5"
replace `var' = "06" if `var' == "6"
replace `var' = "07" if `var' == "7"
replace `var' = "08" if `var' == "8"
replace `var' = "09" if `var' == "9"
}

gen time = fecha3 + "-" + fecha1 + "-" + fecha2

drop fecha*

save TEMP_BaseMonetaria.dta, replace

* Emprolijamos tipo de cambio MEP.

import delimited "https://clasico.rava.com/empresas/precioshistoricos.php?e=DOLAR%20MEP&csv=1", clear

keep fecha cierre
rename fecha time

save TEMP_TipoDeCambio.dta

************************** 
* Abrimos los datos de Alberto.

import delimited "Index_Sentimiento_AF.csv", clear 

* Juntamos variables económicas.

rename date time

merge 1:1 time using TEMP_Reservas.dta
drop if _merge == 2
drop _merge

merge 1:1 time using TEMP_TasaInt.dta
drop if _merge == 2
drop _merge

merge 1:1 time using TEMP_BaseMonetaria.dta
drop if _merge == 2
drop _merge

merge 1:1 time using TEMP_TipoDeCambio.dta
drop if _merge == 2
drop _merge

* Guardamos el un archivo temporario y eliminamos los otros archivos ya utilizados.

save half_full.dta, replace

erase TEMP_Reservas.dta
erase TEMP_TasaInt.dta
erase TEMP_BaseMonetaria.dta
erase TEMP_TipoDeCambio.dta

* Ahora importamos las variables sanitarias.

use "CovidAndVaccines.dta", clear
rename fecha time
save TEMP_Covid.dta, replace

use half_full.dta, clear
merge 1:1 time using TEMP_Covid.dta
drop if _merge == 2
drop _merge
drop terapia edad sex

save half_full2.dta, replace
erase half_full.dta

* Importamos variables climáticas.
	
import delimited "Weather_Buenos_Aires.csv", clear

keep date_time maxtempc mintempc
rename date_time time

save TEMP_Weather.dta, replace

* Casos de covid en el mundo.

import delimited "daily-covid-cases-deaths.csv", clear
keep if code == "OWID_WRL"

rename dailynewconfirmedcasesofcovid19 world_cases
rename dailynewconfirmeddeathsduetocovi world_deaths
drop entity code

rename day time

save TEMP_World.dta, replace

* Finalmente junto lo que nos queda

use half_full2.dta, clear

merge 1:1 time using TEMP_Weather.dta
drop if _merge == 2
drop _merge

merge 1:1 time using TEMP_World.dta
drop if _merge == 2
drop _merge

* Agrego las tendencias del mercado.

merge 1:1 time using Sentiment_TwArgentina.dta
drop _merge

erase TEMP_World.dta
erase TEMP_Weather.dta
erase TEMP_Covid.dta

* Finalmente, arreglamos cosas de formato.

foreach var of varlist caso muerto vacuna world_cases world_deaths{
replace `var' = 0 if `var' == .
}

gen date = daily(time, "YMD")
format date %td
tsset date
order date

* A las variables económicas que tienen missing value les agregamos el valor anterior.

foreach var of varlist tasa base reservas{
gen x = 1 if `var' == .
replace `var' = `var'[_n-1] if x == 1
drop x
}

rename favourite twfav
rename retweet twret
rename pred sentiment
rename ma4 sentsmooth
rename reservas reservasbcra
rename tasa tasaint
rename base basemon
rename cierre tcdolar
rename caso casosarg
rename muerto muertosarg
rename vacuna vacunasarg
rename maxtempc maxtemp
rename mintempc mintemp
rename world_cases casosmundo
rename world_deaths muertesmundo

order time date sentiment sentsmooth

foreach var of varlist _all{
label var `var' " "
}

erase half_full2.dta

* Exportamos los datos finales.

* En .dta.

save Data_FP_Final_Kleiman_Pacheco_Riquelme.dta

* En .csv

export delimited using "Data_FP_Final_Kleiman_Pacheco_Riquelme", replace
