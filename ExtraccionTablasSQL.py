# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""
#Librerias

import pyodbc #libreria para conectar y realizar consultas en la base de datos de SQl Server
import pandas as pd #Libreria para manipulación y análisis de datos
import sqlite3 #implementa un motor de base de datos SQL
import psycopg2 #es el adaptador de base de datos PostgreSQL
import numpy as np

#Arreglos
ARN, Path, Fecha_Entrada_Cava, Fecha_ultima_Prueba, Estado_De_Disponibilidad, Clase_De_Germinacion, Cantidad_De_Semilla, Numero_De_Inventario, Cava, Especie, Id_Site, name, Fecha_De_Origen, Porcentaje_Normal, ID_Inventori_IA = [], [], [], [], [], [], [], [], [], [], [], [], [], [], []

#Paths_group = pd.read_csv("monitor\Paths.csv", sep= ";", header=0, encoding='windows-1252')
Cultivos_Fao = pd.read_csv("monitor\Cultivos_Fao.csv", sep= ";", header=0)
CropGroups = pd.read_csv("monitor\Crop groups.csv", sep= ";", header=0)


#Conexion
server = 'DESKTOP-ETVOI45\SQLEXPRESS' 
database = 'GrinGlobal_bgv' 
username = 'sa' 
password = 'L4b0r4t0r10**' 


cnxn = pyodbc.connect('DRIVER={ODBC Driver 17 for SQL Server};SERVER='+server+';DATABASE='+database+';UID='+username+';PWD='+ password)


#Consulta
cursor = cnxn.cursor()

# Cursor para extrar datos de la tabla accesiones 
accession_id_A = []
inventory_id_A = []
cursor.execute("SELECT A.accession_id, I.inventory_id, I.inventory_number_part1, I.inventory_number_part2, I.inventory_number_part3, AO.source_date, A.accession_number_part1, A.accession_number_part2, A.accession_number_part3, I.quantity_on_hand, I.availability_status_code,  IV.percent_normal, IV.tested_date, TS.name FROM inventory I INNER JOIN accession A on A.accession_id = I.accession_id, taxonomy_species TS, accession_source AO, inventory_viability IV WHERE I.form_type_code='SD' AND  a.taxonomy_species_id = TS.taxonomy_species_id AND a.accession_id = ao.accession_id AND i.inventory_id = IV.inventory_id  order by A.taxonomy_species_id;")
accesion = cursor.fetchall()
for accesion in accesion:
    ARN.append("%s"%(accesion[6])+ "%s"%(accesion[7]) + "%s"%(accesion[8]))
    Especie.append(accesion[13])
    Cantidad_De_Semilla.append(accesion[9])
    Estado_De_Disponibilidad.append(accesion[10])
    Numero_De_Inventario.append(accesion[3])
    Cava.append(accesion[4])
    accession_id_A.append(accesion[0])
    inventory_id_A.append(accesion[1])
    Porcentaje_Normal.append(accesion[11])
    Fecha_ultima_Prueba.append(accesion[12])
    Fecha_De_Origen.append(accesion[5])
  
#creamos dataframes a partir de los arreglos creados en la consulta a la base de datos 
ARN = pd.DataFrame(ARN)
Especie = pd.DataFrame(Especie)
Cantidad_De_Semilla = pd.DataFrame(Cantidad_De_Semilla)
Cantidad_De_Semilla.fillna(0, inplace = True)
Cantidad_De_Semilla = Cantidad_De_Semilla.astype(int)
Estado_De_Disponibilidad = pd.DataFrame(Estado_De_Disponibilidad)
Numero_De_Inventario = pd.DataFrame(Numero_De_Inventario)
Cava = pd.DataFrame(Cava)
accession_id_A = pd.DataFrame(accession_id_A)
inventory_id_A = pd.DataFrame(inventory_id_A)
Porcentaje_Normal = pd.DataFrame(Porcentaje_Normal)
Porcentaje_Normal.fillna(0, inplace = True)
Fecha_ultima_Prueba = pd.DataFrame(Fecha_ultima_Prueba)
Fecha_De_Origen = pd.DataFrame(Fecha_De_Origen)

#Categorizacion de las variables cantidad de semillas y germinacion 
for i in range(0,len(Cantidad_De_Semilla)):
    if Cantidad_De_Semilla[0][i] < 1500:
       Cantidad_De_Semilla[0][i] = 1
    elif 1500 <= Cantidad_De_Semilla[0][i] <= 3000:
        Cantidad_De_Semilla[0][i] = 2
    elif Cantidad_De_Semilla[0][i] > 3000:
        Cantidad_De_Semilla[0][i] = 3
 
        
for p in range(0,len(Porcentaje_Normal)):
    if Porcentaje_Normal[0][p] < 65:
       Porcentaje_Normal[0][p] = 1
    elif 65 <= Porcentaje_Normal[0][p] <= 85:
        Porcentaje_Normal[0][p] = 2
    elif Porcentaje_Normal[0][p] > 85:
        Porcentaje_Normal[0][p] = 3
        
for ed in range(0, len(Estado_De_Disponibilidad)):
    if Estado_De_Disponibilidad[0][ed] == 'REMOVED':
        Estado_De_Disponibilidad[0][ed] = 1
    if Estado_De_Disponibilidad[0][ed] == 'NOT VIABLE':
        Estado_De_Disponibilidad[0][ed] = 2
    if Estado_De_Disponibilidad[0][ed] == 'NO-INV':
        Estado_De_Disponibilidad[0][ed] = 3
    if Estado_De_Disponibilidad[0][ed] == 'NO-ACC':
        Estado_De_Disponibilidad[0][ed] = 4
    if Estado_De_Disponibilidad[0][ed] == 'NEEDSGERM':
        Estado_De_Disponibilidad[0][ed] = 5
    if Estado_De_Disponibilidad[0][ed] == 'NA':
        Estado_De_Disponibilidad[0][ed] = 6
    if Estado_De_Disponibilidad[0][ed] == 'LOWPOP':
        Estado_De_Disponibilidad[0][ed] = 7
    if Estado_De_Disponibilidad[0][ed] == 'LOWGERM':
        Estado_De_Disponibilidad[0][ed] = 8
    if Estado_De_Disponibilidad[0][ed] == 'LOW':
        Estado_De_Disponibilidad[0][ed] = 9
    if Estado_De_Disponibilidad[0][ed] == 'ELIMIN':
        Estado_De_Disponibilidad[0][ed] = 10
    if Estado_De_Disponibilidad[0][ed] == 'DUPLICATE':
        Estado_De_Disponibilidad[0][ed] = 11
    if Estado_De_Disponibilidad[0][ed] == 'DESP_BAJ':
        Estado_De_Disponibilidad[0][ed] = 12
    if Estado_De_Disponibilidad[0][ed] == 'CURAT_ATTN':
        Estado_De_Disponibilidad[0][ed] = 13
    if Estado_De_Disponibilidad[0][ed] == 'BAJ_GER_POB':
        Estado_De_Disponibilidad[0][ed] = 14
    if Estado_De_Disponibilidad[0][ed] == 'AVAIL':
        Estado_De_Disponibilidad[0][ed] = 15


ARN_Accesion = pd.concat([accession_id_A, inventory_id_A, ARN, Especie, Cantidad_De_Semilla, Estado_De_Disponibilidad, Numero_De_Inventario, Cava, Fecha_De_Origen, Fecha_ultima_Prueba, Porcentaje_Normal], axis = 1)
DF_Accesion = pd.DataFrame(ARN_Accesion)
DF_Accesion.columns = ["Accession_id", "Inventory_id", "ARN", "Especie", "Cantidad_De_Semilla", "Estado_De_Disponibilidad", "Numero_De_Inventario", "Cava", "Fecha_De_Origen", "Fecha_ultima_Prueba", "Porcentaje_Normal"]

DF_Accesion.fillna('0', inplace=True)
#%%
# Cursor para extraer datos de  inventory_action
cursor.execute("SELECT A.inventory_id, A.started_date FROM inventory I INNER JOIN inventory_action A on A.inventory_id = \
               I.accession_id WHERE I.form_type_code='SD' and action_name_code = 'ENTRADA_CAVAS' order by A.inventory_id")
action = cursor.fetchall()
for action in action:
    ID_Inventori_IA.append(action[0])
    Fecha_Entrada_Cava.append(action[1]) 
    
#creamos dataframes a partir de los arreglos creados en la consulta a la base de datos 
ID_Inventori_IA = pd.DataFrame(ID_Inventori_IA)
Fecha_Entrada_Cava = pd.DataFrame(Fecha_Entrada_Cava) 
FeEntrCava = pd.concat([ID_Inventori_IA, Fecha_Entrada_Cava], axis = 1)
FeEntrCava.columns = ["Inventory_id", "Fecha_Entrada_Cava"]
#%%Cursor para extraer datos de  inventory_action
cursor.execute("SELECT site_long_name, site_id FROM [GrinGlobal_bgv].[dbo].[site]" )
site = cursor.fetchall()
for site in site:
    Id_Site.append(site[1])
    name.append(site[0])  
    
#%%Desconexion    
#cursor.commit() #Guarda los cambios en la base de datos de SQL server 
#cursor.close()  


# Crear un vector con los datos de cultivo y por ahora vector con valores de [ortodoxa, recalcitrante e intermedia]
Fe_Entrada_Cava = np.zeros(len(DF_Accesion["Inventory_id"]))
Cultivo_Group = np.zeros(len(DF_Accesion["Inventory_id"])) 
Grupo_Group = np.zeros(len(DF_Accesion["Inventory_id"]))
CatSemilla_Group = np.zeros(len(DF_Accesion["Inventory_id"]))

#convirtiendo arreglos en Dataframes
Fe_Entrada_Cava = pd.DataFrame(Fe_Entrada_Cava)
Cultivo_Group = pd.DataFrame(Cultivo_Group)
Grupo_Group = pd.DataFrame(Grupo_Group)
CatSemilla_Group = pd.DataFrame(CatSemilla_Group)

#Se validadn los inventory Id de La tabla de accionde inventario y los de la tabla de inventario para asignar la fecha 
#a las accesiones de inventario
for f in range(0,len(FeEntrCava["Inventory_id"])):
    for ff in range(0, len(DF_Accesion["Inventory_id"])):
        if FeEntrCava["Inventory_id"][f] == DF_Accesion["Inventory_id"][ff]:
            Fe_Entrada_Cava[0][ff] = FeEntrCava["Fecha_Entrada_Cava"][f]          
            

#%%creacion de las variables longevidad y ultimomonitores para vizualizacion

#Crear linea temporal para fechas incompletas a primero de julio... y para mes tomar el 1 de cada mes 


from datetime import datetime

fechaActual = datetime.today()
longevidad = np.zeros(len(Fe_Entrada_Cava[0]))
ultimoMonitoreo = np.zeros(len(Fecha_ultima_Prueba[0]))


for l in range(0, (len(Fe_Entrada_Cava[0]))):
    if Fecha_ultima_Prueba[0][l] == 0.0:
        ultimoMonitoreo[l] = 0
    else:
        fultimoMonitoreo = datetime.strptime("%s"%(Fecha_ultima_Prueba[0][l]), '%Y-%m-%d %H:%M:%S')
        
        #condicional para dias y mes no existente
        
        ffAnioUM = fechaActual.year - fultimoMonitoreo.year
        ffMesUM = fechaActual.month - fultimoMonitoreo.month
        ffDiasUM = fechaActual.day - fultimoMonitoreo.day
        
        if ffMesUM < 0:
            ffMesUM = 12 + ffMesUM 
            ffAnioUM = ffAnioUM - 1
        if ffDiasUM < 0:
            ffDiasUM = 30 + ffDiasUM
            ffMesUM = ffMesUM - 1
        
        #Categorizacion de la variable ultimo monitorep
        if round((ffAnioUM*365 + (ffMesUM*30) + (ffDiasUM)), 2) > 1825:
            ultimoMonitoreo[l] = 1
        if 1095 < round((ffAnioUM*365 + (ffMesUM*30) + (ffDiasUM)), 2) <= 1825:
            ultimoMonitoreo[l] = 2
        if round((ffAnioUM*365 + (ffMesUM*30) + (ffDiasUM)), 2) <= 1095:
            ultimoMonitoreo[l] = 3
       
        
    if Fe_Entrada_Cava[0][l] == 0.0:
        longevidad[l] = 0
    else:
        fechaEntrC = datetime.strptime("%s"%(Fe_Entrada_Cava[0][l]), '%Y-%m-%d %H:%M:%S')
        
        #condicional para dias y mes no existente
        
        ffAnio = fechaActual.year - fechaEntrC.year
        ffMes = fechaActual.month - fechaEntrC.month
        ffDias = fechaActual.day - fechaEntrC.day
        
        if ffMes < 0:
            ffMes = 12 + ffMes 
            ffAnio = ffAnio - 1
        if ffDias < 0:
            ffDias = 30 + ffDias
            ffMes = ffMes - 1
            
        #Categorizacion de la variable longeviad
        if round((ffAnio*365 + (ffMes*30) + (ffDias)), 2) > 7300:
            longevidad[l] = 1
        if 3650 < round((ffAnio*365 + (ffMes*30) + (ffDias)), 2) <= 7300:
            longevidad[l] = 2
        if round((ffAnio*365 + (ffMes*30) + (ffDias)), 2) <= 3650:
            longevidad[l] = 3
            
longevidad = pd.DataFrame(longevidad)
ultimoMonitoreo = pd.DataFrame(ultimoMonitoreo)

#%%Creacion de paths     

#definicion de Frames para asignacion de paths
VarCava = pd.DataFrame({'Cava':['Cava Desconocida', '-20° C', '-10° C', '0° C'], 'Id_Cava': [10, 20, 30 , 40]})

#Unificar formatos de la base de datos
#Unicode para mostrar gardos en shiny

idcava = np.zeros(len(DF_Accesion['Cava']))

for ca in range(0, len(DF_Accesion['Cava'])):
    if DF_Accesion['Cava'][ca][0:3] == "0°C":
        idcava[ca] = (VarCava['Id_Cava'][3])
    elif DF_Accesion['Cava'][ca][0:5] == '-20°C':
        idcava[ca] = (VarCava['Id_Cava'][1])
    elif DF_Accesion['Cava'][ca][0:6] == '\xad-20°C':
        idcava[ca] = (VarCava['Id_Cava'][1])
    elif DF_Accesion['Cava'][ca][0:4] == '-20°':
        idcava[ca] = (VarCava['Id_Cava'][1])
    elif DF_Accesion['Cava'][ca][0:6] == '-10 ºC':
        idcava[ca] = (VarCava['Id_Cava'][2])
    elif DF_Accesion['Cava'][ca][0:3] == '4°C':
        idcava[ca] = (VarCava['Id_Cava'][3])
    else:
        idcava[ca] = (VarCava['Id_Cava'][0])

idcava = pd.DataFrame(idcava)

Paths = np.zeros(len(Estado_De_Disponibilidad[0]))
for path in range(0, len(DF_Accesion['Especie'])):
    for esp in range(0, len(Cultivos_Fao['Especie'])):
        if DF_Accesion['Especie'][path] == Cultivos_Fao['Especie'][esp]:
            Paths[path] = f'{Cultivos_Fao["Id_Cultivo"][esp]}{Cultivos_Fao["Id_Especie"][esp]}{idcava[0][path]}'


usryr = np.random.rand(len(DF_Accesion['Cava'])) *10
Id_Centro = np.zeros(len(Estado_De_Disponibilidad[0]))

usryr = round(pd.DataFrame(usryr), 2)
Id_Centro = pd.DataFrame(Id_Centro)
Paths = pd.DataFrame(Paths)
#%%           
Expanded_collection_data = pd.concat([DF_Accesion['ARN'], Paths, usryr, DF_Accesion['Estado_De_Disponibilidad'], DF_Accesion['Porcentaje_Normal'], DF_Accesion['Cantidad_De_Semilla'], DF_Accesion['Numero_De_Inventario'], DF_Accesion['Cava'], DF_Accesion['Especie'], Id_Centro, longevidad, ultimoMonitoreo], axis = 1)
Expanded_collection_data.columns = ["ARN", "PATH", "Use/yr", "Estado de Disponibilidad", "Germinacion", "Cantidad de Semillas", "Numero_De_Inventario", "Cava", "Especie", "Id del Centro", "Longevidad", "Ultimo Monitoreo"]

Expanded_collection_data.to_csv("Expandedcollection_data.csv")