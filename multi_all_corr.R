


require(nsga2R)
require(dplyr)
require(ggplot2)


# FELIPE RIVAS 28/03/2025 FELIPE.RIVAS5492@GMAIL.COM
# :D

# FELIPE RIVAS 22/09/2025 00:35 HRS SE TIENE LA CALIBRACION PARA EL MODELO EMBALSE (R) Y CUERPO DE AGUA (LR) CON CORRECCION DE FORZANTES
# PARA ARCHIVOS SCF O CUALQUIER OTRO CODIGO CONSULTAR AL CORREO POR FAVOR.
# 


#### DIRECCIONES NECESARIAS ##### 


rvp_file_path_salida<- ".../RAVEN1/00_modelos/resultados_def_1_LR_corr/Villarrica_GR4J.rvp"
raven_executable_salida<- ".../RAVEN1/RavenExecutableWin64_v3.8/Raven.exe"
resultados_dir_salida<- ".../RAVEN1/00_modelos/resultados_def_1_LR_corr"
nombre_base_rvi <- "Villarrica_GR4J"

rvp_file_path_entrada <- ".../RAVEN1/00_modelos/resultados_def_2_LR_corr/Villarrica_GR4J.rvp"
raven_executable_entrada <- ".../RAVEN1/RavenExecutableWin64_v3.8/Raven.exe"
resultados_dir_entrada <- ".../00_modelos/resultados_def_2_LR_corr"
nombre_base_rvi <- "Villarrica_GR4J"


nombres_archivos <- c(
  "AET_Daily_Average_ByHRU.csv",
  "AET_Monthly_CumulSum_BySubbasin.csv",
  "Demands.csv",
  "Diagnostics.csv",
  "ExhaustiveMassBalance.csv",
  "ForcingFunctions.csv",
  "Hydrographs.csv",
  "PRECIP_Monthly_CumulSum_BySubbasin.csv",
  "Raven_errors.txt",
  "ReservoirStages.csv",
  "RUNOFF_Continuous_Average_ByHRU.csv",
  "solution.rvc",
  "SubbasinProperties.csv",
  "WaterLevels.csv",
  "WatershedMassEnergyBalance.csv",
  "WatershedStorage.csv",
  "SNOW_Continuous_Average_ByHRU.csv",
  "PRECIP_Continuous_Average_ByHRU.csv",
  "PRECIP_WYearly_CumulSum_ByHRU.csv",
  "SOIL[0]_Daily_Average_ByHRU.csv",
  "SOIL[1]_Monthly_Maximum_BySubbasin.csv",
  "SOIL[2]_Monthly_Maximum_BySubbasin.csv",
  "SOIL[3]_Monthly_Maximum_BySubbasin.csv",
  "SOIL[4]_Monthly_Maximum_BySubbasin.csv",
  "TEMP_MAX_Continuous_Average_ByHRU.csv",
  "TEMP_MIN_Continuous_Average_ByHRU.csv"
)


#### FUNCIONES LR  #### 




ejecutar_raven <- function(raven_executable, resultados_dir, nombre_base_rvi) {
  # Construir la ruta completa al archivo .rvi
  archivo_rvi <- file.path(resultados_dir, nombre_base_rvi)
  
  # Crear el comando para ejecutar Raven
  comando <- sprintf('"%s" "%s"', raven_executable, archivo_rvi)
  
  # # Imprimir el comando para depuración
  # cat("Comando a ejecutar: ", comando, "\n")
  
  # Ejecutar el comando y capturar el resultado
  resultado <- system(comando, intern = TRUE)
  
  # Retornar el resultado de la ejecución para más análisis si es necesario
  return(resultado)
}


ejecutar_raven(raven_executable_entrada, resultados_dir_entrada, nombre_base_rvi) 


eliminar_archivos <- function(carpeta, nombres_archivos) {
  # Recorre cada nombre de archivo en la lista
  for (nombre_archivo in nombres_archivos) {
    # Construye la ruta completa del archivo
    ruta_completa <- file.path(carpeta, nombre_archivo)
    
    # Verifica si el archivo existe y lo elimina
    if (file.exists(ruta_completa)) {
      unlink(ruta_completa)
    }
  }
}

# CUALQUIER PARAMETRO ADICIONAL A AGREGAR QUE ESTE EN UNA SECCION DIFERENTE HAY QUE AGREGARLO AQUI. 

modify_gr4j_params_from_list <- function(rvp_file_path, param_list) {
  
  
  
  lines <- readLines(rvp_file_path)
  
  # State variables to identify relevant sections
  reading_soil <- FALSE
  reading_land_use <- FALSE
  reading_global_params <- FALSE
  
  # Variables to store the parameter names in the lists
  soil_param_names <- NULL
  land_use_param_names <- NULL
  
  # Loop through each line in the file
  for (i in seq_along(lines)) {
    line <- lines[i]
    
    # Identify the start of the SoilParameterList section
    if (grepl(":SoilParameterList", line)) {
      reading_soil <- TRUE
      # Extract the parameter names
      param_line <- lines[i + 1]
      soil_param_names <- unlist(strsplit(param_line, ","))[-1]  # Exclude ':Parameters'
      soil_param_names <- trimws(soil_param_names)
      next
    }
    
    # Identify the start of the LandUseParameterList section
    if (grepl(":LandUseParameterList", line)) {
      reading_land_use <- TRUE
      # Extract the parameter names
      param_line <- lines[i + 1]
      land_use_param_names <- unlist(strsplit(param_line, ","))[-1]  # Exclude ':Parameters'
      land_use_param_names <- trimws(land_use_param_names)
      next
    }
    
    # Identify the end of sections
    if (grepl(":EndSoilParameterList", line)) {
      reading_soil <- FALSE
    }
    
    if (grepl(":EndLandUseParameterList", line)) {
      reading_land_use <- FALSE
    }
    
    # Modify parameters in the SoilParameterList section
    if (reading_soil && grepl(",", line)) {
      parts <- unlist(strsplit(line, ","))
      name <- trimws(parts[1])
      
      # Check if the name is in the list of soil parameters
      if (name %in% names(param_list$soil_parameters)) {
        soil_params <- param_list$soil_parameters[[name]]
        
        # Loop through the parameter names and update if applicable
        for (param_name in names(soil_params)) {
          param_index <- which(soil_param_names == param_name) + 1  # +1 for the soil name
          if (length(param_index) > 0 && param_index <= length(parts)) {
            parts[param_index] <- as.character(soil_params[[param_name]])
          }
        }
        # Update the line in the lines vector
        lines[i] <- paste(parts, collapse = ",")
      }
    }
    
    # Modify parameters in the LandUseParameterList section
    if (reading_land_use && grepl(",", line)) {
      parts <- unlist(strsplit(line, ","))
      name <- trimws(parts[1])
      
      # Check if the name is in the list of land use parameters
      if (name %in% names(param_list$land_use_parameters)) {
        land_use_params <- param_list$land_use_parameters[[name]]
        
        # Loop through the parameter names and update if applicable
        for (param_name in names(land_use_params)) {
          param_index <- which(land_use_param_names == param_name) + 1  # +1 for the land use name
          if (length(param_index) > 0 && param_index <= length(parts)) {
            parts[param_index] <- as.character(land_use_params[[param_name]])
          }
        }
        # Update the line in the lines vector
        lines[i] <- paste(parts, collapse = ",")
      }
      
      # Modify MELT_FACTOR and DD_MELT_TEMP for 'Ice1'
      if (name == "Ice1" && "melt_factor_parameters" %in% names(param_list)) {
        melt_factor_params <- param_list$melt_factor_parameters[[name]]
        
        # Update MELT_FACTOR
        param_index_melt <- which(land_use_param_names == "MELT_FACTOR") + 1
        if (length(param_index_melt) > 0 && param_index_melt <= length(parts)) {
          parts[param_index_melt] <- as.character(melt_factor_params["MELT_FACTOR"])
        }
        
        # Update DD_MELT_TEMP
        param_index_dd_melt <- which(land_use_param_names == "DD_MELT_TEMP") + 1
        if (length(param_index_dd_melt) > 0 && param_index_dd_melt <= length(parts)) {
          parts[param_index_dd_melt] <- as.character(melt_factor_params["DD_MELT_TEMP"])
        }
        
        # Update the line in the lines vector
        lines[i] <- paste(parts, collapse = ",")
      }
      
      # Repeat for 'Ice2'
      if (name == "Ice2" && "melt_factor_parameters" %in% names(param_list)) {
        melt_factor_params <- param_list$melt_factor_parameters[[name]]
        
        # Update MELT_FACTOR
        param_index_melt <- which(land_use_param_names == "MELT_FACTOR") + 1
        if (length(param_index_melt) > 0 && param_index_melt <= length(parts)) {
          parts[param_index_melt] <- as.character(melt_factor_params["MELT_FACTOR"])
        }
        
        # Update DD_MELT_TEMP
        param_index_dd_melt <- which(land_use_param_names == "DD_MELT_TEMP") + 1
        if (length(param_index_dd_melt) > 0 && param_index_dd_melt <= length(parts)) {
          parts[param_index_dd_melt] <- as.character(melt_factor_params["DD_MELT_TEMP"])
        }
        
        # Update the line in the lines vector
        lines[i] <- paste(parts, collapse = ",")
      }
      
      # Update for 'Ice3' 
      if (name == "Ice3" && "melt_factor_parameters" %in% names(param_list)) {
        melt_factor_params <- param_list$melt_factor_parameters[[name]]
        
        # Update MELT_FACTOR
        param_index_melt <- which(land_use_param_names == "MELT_FACTOR") + 1
        if (length(param_index_melt) > 0 && param_index_melt <= length(parts)) {
          parts[param_index_melt] <- as.character(melt_factor_params["MELT_FACTOR"])
        }
        
        # Update DD_MELT_TEMP
        param_index_dd_melt <- which(land_use_param_names == "DD_MELT_TEMP") + 1
        if (length(param_index_dd_melt) > 0 && param_index_dd_melt <= length(parts)) {
          parts[param_index_dd_melt] <- as.character(melt_factor_params["DD_MELT_TEMP"])
        }
        
        
        # Update the line in the lines vector
        lines[i] <- paste(parts, collapse = ",")
      }
      
      
      if (name == "WATER" && "lake_rel_parameters" %in% names(param_list)) {
        lake_params <- param_list$lake_rel_parameters[[name]]
        
        # Update LAKE_REL_COEFF
        param_index_lake <- which(land_use_param_names == "LAKE_REL_COEFF") + 1
        if (length(param_index_lake) > 0 && param_index_lake <= length(parts)) {
          parts[param_index_lake] <- as.character(lake_params["LAKE_REL_COEFF"])
        }
        
        # Update the line in the lines vector
        lines[i] <- paste(parts, collapse = ",")
      }
      
      
      
    }
    
    # Modify global parameters
    if (grepl(":GlobalParameter", line)) {
      # Extract the parameter name and value
      param_parts <- unlist(strsplit(line, "\\s+"))
      if (length(param_parts) >= 2) {
        param_name <- param_parts[2]
        if (param_name %in% names(param_list$global_parameters)) {
          new_value <- as.character(param_list$global_parameters[[param_name]])
          # Reconstruct the line with the new value
          lines[i] <- paste(":GlobalParameter", param_name, new_value)
        }
      }
    }
  }
  
  writeLines(lines, rvp_file_path)
}


lines <- readLines(rvp_file_path_salida)

# TAMBIEN AQUI 
generate_param_list <- function(x) {
  param_list <- list(
    soil_parameters = list(
      # Perfiles de suelo (30 en total)
      "Aporte_Lateral_0_1" = c(POROSITY = x[79], GR4J_X2 = x[1], GR4J_X3 = x[40]),
      "Aporte_Lateral_1_2" = c(POROSITY = x[80], GR4J_X2 = x[2], GR4J_X3 = x[41]),
      "Aporte_Quilque_0_1" = c(POROSITY = x[81], GR4J_X2 = x[3], GR4J_X3 = x[42]),
      "Aporte_Quilque_1_2" = c(POROSITY = x[82], GR4J_X2 = x[4], GR4J_X3 = x[43]),
      "Pie_Villarrica_0_1" = c(POROSITY = x[83], GR4J_X2 = x[5], GR4J_X3 = x[44]),
      "Pie_Villarrica_1_2" = c(POROSITY = x[84], GR4J_X2 = x[6], GR4J_X3 = x[45]),
      "Punta_Villarrica_1_0_1" = c(POROSITY = x[85], GR4J_X2 = x[7], GR4J_X3 = x[46]),
      "Punta_Villarrica_1_1_2" = c(POROSITY = x[86], GR4J_X2 = x[8], GR4J_X3 = x[47]),
      "Punta_Villarrica_2_0_1" = c(POROSITY = x[87], GR4J_X2 = x[9], GR4J_X3 = x[48]),
      "Punta_Villarrica_2_1_2" = c(POROSITY = x[88], GR4J_X2 = x[10], GR4J_X3 = x[49]),
      "Punta_Villarrica_3_0_1" = c(POROSITY = x[89], GR4J_X2 = x[11], GR4J_X3 = x[50]),
      "Punta_Villarrica_3_1_2" = c(POROSITY = x[90], GR4J_X2 = x[12], GR4J_X3 = x[51]),
      "Aporte_Central_Quelhue_0_1" = c(POROSITY = x[91], GR4J_X2 = x[13], GR4J_X3 = x[52]),
      "Aporte_Central_Quelhue_1_2" = c(POROSITY = x[92], GR4J_X2 = x[14], GR4J_X3 = x[53]),
      "Aporte_Reserva_1_2" = c(POROSITY = x[93], GR4J_X2 = x[15], GR4J_X3 = x[54]),
      "Pie_Villarrica_1_0_1" = c(POROSITY = x[94], GR4J_X2 = x[16], GR4J_X3 = x[55]),
      "Pie_Villarrica_1_1_2" = c(POROSITY = x[95], GR4J_X2 = x[17], GR4J_X3 = x[56]),
      "Punta_Villarrica_1_1_0_1" = c(POROSITY = x[96], GR4J_X2 = x[18], GR4J_X3 = x[57]),
      "Punta_Villarrica_1_1_1_2" = c(POROSITY = x[97], GR4J_X2 = x[19], GR4J_X3 = x[58]),
      "Punta_Villarrica_1_2_0_1" = c(POROSITY = x[98], GR4J_X2 = x[20], GR4J_X3 = x[59]),
      "Punta_Villarrica_1_2_1_2" = c(POROSITY = x[99], GR4J_X2 = x[21], GR4J_X3 = x[60]),
      "Punta_Villarrica_1_3_0_1" = c(POROSITY = x[100], GR4J_X2 = x[22], GR4J_X3 = x[61]),
      "Punta_Villarrica_1_3_1_2" = c(POROSITY = x[101], GR4J_X2 = x[23], GR4J_X3 = x[62]),
      "Aporte_Trancura_0_1" = c(POROSITY = x[102], GR4J_X2 = x[24], GR4J_X3 = x[63]),
      "Aporte_Trancura_1_2" = c(POROSITY = x[103], GR4J_X2 = x[25], GR4J_X3 = x[64]),
      "Aporte_Central_Liucura_0_1" = c(POROSITY = x[104], GR4J_X2 = x[26], GR4J_X3 = x[65]),
      "Aporte_Central_Liucura_1_2" = c(POROSITY = x[105], GR4J_X2 = x[27], GR4J_X3 = x[66]),
      "Aporte_Lateral_Liucura_0_1" = c(POROSITY = x[106], GR4J_X2 = x[28], GR4J_X3 = x[67]),
      "Aporte_Lateral_Liucura_1_2" = c(POROSITY = x[107], GR4J_X2 = x[29], GR4J_X3 = x[68]),
      "Aporte_Central_Carhuello_0_1" = c(POROSITY = x[108], GR4J_X2 = x[30], GR4J_X3 = x[69]),
      "Aporte_Central_Carhuello_1_2" = c(POROSITY = x[109], GR4J_X2 = x[31], GR4J_X3 = x[70]),
      "Aporte_Lateral_Carhuello_0_1" = c(POROSITY = x[110], GR4J_X2 = x[32], GR4J_X3 = x[71]),
      "Aporte_Lateral_Carhuello_1_2" = c(POROSITY = x[111], GR4J_X2 = x[33], GR4J_X3 = x[72]),
      "Aporte_Central_Caburgua_0_1" = c(POROSITY = x[112], GR4J_X2 = x[34], GR4J_X3 = x[73]),
      "Aporte_Central_Caburgua_1_2" = c(POROSITY = x[113], GR4J_X2 = x[35], GR4J_X3 = x[74]),
      "Aporte_Lateral_Caburgua_0_1" = c(POROSITY = x[114], GR4J_X2 = x[36], GR4J_X3 = x[75]),
      "Aporte_Lateral_Caburgua_1_2" = c(POROSITY = x[115], GR4J_X2 = x[37], GR4J_X3 = x[76]),
      # Nuevos perfiles agregados
      "aquifer" = c(POROSITY = x[116], GR4J_X2 = x[38], GR4J_X3 = x[77]),
      "temp" = c(POROSITY = x[117], GR4J_X2 = x[39], GR4J_X3 = x[78])
    ),
    land_use_parameters = list(
      "Forest" = c(GR4J_X4 = x[118]),
      "Forest_Ice" = c(GR4J_X4 = x[119]),
      "Grassland" = c(GR4J_X4 = x[120]),
      "Ice1" = c(GR4J_X4 = x[121]),
      "Ice2" = c(GR4J_X4 = x[122]),
      "Ice3" = c(GR4J_X4 = x[123]),
      "OPEN" = c(GR4J_X4 = x[124])
    ),
    melt_factor_parameters = list(
      "Ice1" = c(MELT_FACTOR = x[127], DD_MELT_TEMP = x[131]),
      "Ice2" = c(MELT_FACTOR = x[128], DD_MELT_TEMP = x[132]),
      "Ice3" = c(MELT_FACTOR = x[129], DD_MELT_TEMP = x[133])
    ),
    global_parameters = list(
      "AIRSNOW_COEFF" = x[125],
      "AVG_ANNUAL_SNOW" = x[126],
      "RAINSNOW_TEMP" = x[130]
    ),
    lake_rel_parameters= list(
      "WATER" = c(LAKE_REL_COEFF = x[134])
    )
    
  )
  return(param_list)
}


modificar_y_ejecutar_raven <- function(param_list, rvp_file_path,nombre_base_rvi,resultados_dir,raven_executable) {
  # Definir las rutas y archivos específicos dentro de la función
  modify_gr4j_params_from_list(rvp_file_path, param_list)
  
  # Ejecutar el modelo Raven con el archivo RVP modificado
  resultado <- ejecutar_raven(raven_executable, resultados_dir, nombre_base_rvi)
  
  return(resultado)
}

obtener_kge_nse <- function(ruta_diagnosticos, indice) {
  diagnosticos <- read.csv(ruta_diagnosticos)
  kge <- diagnosticos$DIAG_KLING_GUPTA[indice]
  nse <- diagnosticos$DIAG_NASH_SUTCLIFFE[indice]
  return(list(KGE = kge, NSE = nse))
}

calcular_kge_con_parametros <- function(param_list, rvp_file_path, raven_executable, resultados_dir, nombre_base_rvi, nombres_archivos, indice) {
  eliminar_archivos(resultados_dir, nombres_archivos)
  
  resultado_modelo <- modificar_y_ejecutar_raven(param_list, rvp_file_path, nombre_base_rvi, resultados_dir, raven_executable)
  ruta_diagnosticos <- file.path(resultados_dir, "Diagnostics.csv")
  resutados_all <- obtener_kge_nse(ruta_diagnosticos, indice)
  KGE <- resutados_all$KGE
  print(KGE)
  return(KGE)
  
}

evaluar_modelos <- function(x) {
  param_list = generate_param_list(x)
  
  kge_salida <- calcular_kge_con_parametros(param_list, rvp_file_path_salida, raven_executable_salida, resultados_dir_salida, nombre_base_rvi, nombres_archivos, 3)
  kge_entrada <- calcular_kge_con_parametros(param_list, rvp_file_path_entrada, raven_executable_entrada, resultados_dir_entrada, nombre_base_rvi, nombres_archivos, 4)
  
  return(c(-kge_entrada, -kge_salida))
}




#### CALIBRAR MODELO LR ####

param_limits <- list(
  GR4J_X2_min = -25, GR4J_X2_max = 25,            # Límites para GR4J_X2: P
  GR4J_X3_min = 0.1, GR4J_X3_max = 600,          # Límites para GR4J_X3: P
  GR4J_X4_min = 1, GR4J_X4_max = 100,              # Límites para GR4J_X4: RAVEN P = 0 - 20 
  POROSITY_min = 0.1, POROSITY_max = 0.6,         # Límites para POROSITY RAVEN 
  AIRSNOW_COEFF_min = 0, AIRSNOW_COEFF_max = 0.1, # Límites para AIRSNOW_COEFF : CEMANEIGE 
  AVG_ANNUAL_SNOW_min = 0, AVG_ANNUAL_SNOW_max = 100, # Límites para AVG_ANNUAL_SNOW: CEMANEIGE
  MELT_FACTOR_min = 2, MELT_FACTOR_max = 10,#  Límites para MELT_FACTOR: CEMANEIGE
  RAINSNOW_TEMP_min =0, RAINSNOW_TEMP_max = 0,
  DD_MELT_TEMP_min =0, DD_MELT_TEMP_max = 0,
  LAKE_REL_COEFF_min =0.001, LAKE_REL_COEFF_max = 0.5 # RAVEN MANUAL [1/d]
  
)  # Límites para RAINSNOW_TEMP


# Número total de parámetros, considerando los 6 adicionales:
num_params <- 134  # 30 (GR4J_X2) + 30 (GR4J_X3) + 30 (POROSITY) + 4 (GR4J_X4) + 1 (AIRSNOW_COEFF) + 1 (AVG_ANNUAL_SNOW) + 1 (MELT_FACTOR) + 3 temp + 3 aqufer 


# Generación de los límites para cada parámetro
lowerBounds <- c(
  rep(param_limits$GR4J_X2_min, 39),        # GR4J_X2
  rep(param_limits$GR4J_X3_min, 39),        # GR4J_X3
  rep(param_limits$POROSITY_min, 39),
  rep(param_limits$GR4J_X4_min, 7),
  param_limits$AIRSNOW_COEFF_min,           # AIRSNOW_COEFF
  param_limits$AVG_ANNUAL_SNOW_min,  
  
  
  rep(param_limits$MELT_FACTOR_min, 3),             # MELT_FACTOR  rep(param_limits$MELT_FACTOR_min, 4), 
  param_limits$RAINSNOW_TEMP_min,
  rep(param_limits$DD_MELT_TEMP_min, 3),
  param_limits$LAKE_REL_COEFF_min )


upperBounds <- c(
  rep(param_limits$GR4J_X2_max, 39),        # GR4J_X2
  rep(param_limits$GR4J_X3_max, 39),        # GR4J_X3
  rep(param_limits$POROSITY_max, 39),       # POROSITY
  rep(param_limits$GR4J_X4_max, 7),
  
  param_limits$AIRSNOW_COEFF_max,           # AIRSNOW_COEFF
  param_limits$AVG_ANNUAL_SNOW_max,   
  
  # AVG_ANNUAL_SNOW
  rep(param_limits$MELT_FACTOR_max, 3),             # MELT_FACTOR  rep(param_limits$MELT_FACTOR_min, 4), 
  param_limits$RAINSNOW_TEMP_max,
  rep(param_limits$DD_MELT_TEMP_max, 3),
  param_limits$LAKE_REL_COEFF_max
)


# Imprimir los límites para verificar
cat("Límites inferiores:\n")
print(lowerBounds)
cat("Límites superiores:\n")
print(upperBounds)


# Ejecutar la optimización multiobjetivo con NSGA-II
resultado_nsga2_LR <- nsga2R(
  fn = evaluar_modelos,
  varNo = num_params,
  objDim = 2,
  lowerBounds = lowerBounds,
  upperBounds = upperBounds,
  popSize = 134,  # Tamaño de la población
  generations = 50,  # Número de generaciones para obtener resultados significativos
  cprob = 0.9,  # Probabilidad de cruce
  XoverDistIdx = 20,  # Índice de distribución de cruce
  mprob = 0.2,  # Probabilidad de mutación
  MuDistIdx = 20  # Índice de distribución de mutación
)


par(mfrow = c(1,1))

# Imprimir los resultados de la optimización
print(resultado_nsga2_LR)

# Graficar los resultados de la optimización
plot(-resultado_nsga2_LR$objectives)


directorio_guardado <- "C:/Otono_2024/Memoria/RAVEN1/01_resultados_calib"
nombre_archivo <- "resultado_nsga2_LR_50_def_cf111.rds"

# Guardar el objeto resultado_nsga2 en el archivo .rds
saveRDS(resultado_nsga2_LR, file = file.path(directorio_guardado, nombre_archivo))

cat("El objeto resultado_nsga2 se ha guardado en:", file.path(directorio_guardado, nombre_archivo))



















































#### DIRECCIONES NECESARIAS ##### 


rvp_file_path_salida<- ".../RAVEN1/00_modelos/resultados_def_1_corr/Villarrica_GR4J.rvp"
raven_executable_salida<- ".../RAVEN1/RavenExecutableWin64_v3.8/Raven.exe"
resultados_dir_salida<- ".../RAVEN1/00_modelos/resultados_def_1_corr"
nombre_base_rvi <- "Villarrica_GR4J"
rvh_file_path_salida = ".../RAVEN1/00_modelos/resultados_def_1_corr/Villarrica_GR4J.rvh"

rvp_file_path_entrada <- ".../RAVEN1/00_modelos/resultados_def_2_corr/Villarrica_GR4J.rvp"
raven_executable_entrada <- ".../RAVEN1/RavenExecutableWin64_v3.8/Raven.exe"
resultados_dir_entrada <- ".../RAVEN1/00_modelos/resultados_def_2_corr"
nombre_base_rvi <- "Villarrica_GR4J"
rvh_file_path_entrada = ".../RAVEN1/00_modelos/resultados_def_2_corr/Villarrica_GR4J.rvh"


nombres_archivos <- c(
  "AET_Daily_Average_ByHRU.csv",
  "AET_Monthly_CumulSum_BySubbasin.csv",
  "Demands.csv",
  "Diagnostics.csv",
  "ExhaustiveMassBalance.csv",
  "ForcingFunctions.csv",
  "Hydrographs.csv",
  "PRECIP_Monthly_CumulSum_BySubbasin.csv",
  "Raven_errors.txt",
  "ReservoirStages.csv",
  "RUNOFF_Continuous_Average_ByHRU.csv",
  "solution.rvc",
  "SubbasinProperties.csv",
  "WaterLevels.csv",
  "WatershedMassEnergyBalance.csv",
  "WatershedStorage.csv",
  "SNOW_Continuous_Average_ByHRU.csv",
  "PRECIP_Continuous_Average_ByHRU.csv",
  "PRECIP_WYearly_CumulSum_ByHRU.csv",
  "SOIL[0]_Daily_Average_ByHRU.csv",
  "SOIL[1]_Monthly_Maximum_BySubbasin.csv",
  "SOIL[2]_Daily_Average_ByHRU.csv",
  "SOIL[3]_Monthly_Maximum_BySubbasin.csv",
  "TEMP_MAX_Continuous_Average_ByHRU.csv",
  "TEMP_MIN_Continuous_Average_ByHRU.csv"
)


#### FUNCIONES R 



#### FUNCIONES R #### 

ejecutar_raven <- function(raven_executable, resultados_dir, nombre_base_rvi) {
  # Construir la ruta completa al archivo .rvi
  archivo_rvi <- file.path(resultados_dir, nombre_base_rvi)
  
  # Crear el comando para ejecutar Raven
  comando <- sprintf('"%s" "%s"', raven_executable, archivo_rvi)
  
  # # Imprimir el comando para depuración
  # cat("Comando a ejecutar: ", comando, "\n")
  
  # Ejecutar el comando y capturar el resultado
  resultado <- system(comando, intern = TRUE)
  
  # Retornar el resultado de la ejecución para más análisis si es necesario
  return(resultado)
}

ejecutar_raven(raven_executable_entrada, resultados_dir_entrada, nombre_base_rvi) 
ejecutar_raven(raven_executable_salida, resultados_dir_salida, nombre_base_rvi) 

eliminar_archivos <- function(carpeta, nombres_archivos) {
  # Recorre cada nombre de archivo en la lista
  for (nombre_archivo in nombres_archivos) {
    # Construye la ruta completa del archivo
    ruta_completa <- file.path(carpeta, nombre_archivo)
    
    # Verifica si el archivo existe y lo elimina
    if (file.exists(ruta_completa)) {
      unlink(ruta_completa)
    }
  }
}


# ALGUNOS PARAMETROS DE LAGO SALEN MEJOR MODIFICARLOS DESDE ESTA FUNCION QUE DE MODIFY_GR4J.
WeirCoef <- function(rvh_file_path,x,y) {
  # Leer el archivo
  lines <- readLines(rvh_file_path)
  
  # Variable de estado para identificar si estamos en la sección de "Reservoir"
  in_reservoir_section <- FALSE

  
  # Recorrer las líneas del archivo
  for (i in seq_along(lines)) {
    line <- lines[i]
    
    # Detectar el inicio de la sección de "Reservoir"
    if (grepl(":Reservoir", line)) {
      in_reservoir_section <- TRUE
    }
    
    # Detectar el fin de la sección de "Reservoir"
    if (in_reservoir_section && grepl(":EndReservoir", line)) {
      in_reservoir_section <- FALSE
    }
    
    # Modificar el coeficiente de Weir en la sección de "Reservoir"
    if (in_reservoir_section && grepl(":WeirCoefficient", line)) {
      # Reemplazar el valor actual de WeirCoefficient por x
      lines[i] <- paste(":WeirCoefficient", x)
    }
    
    # Modificar el coeficiente de Weir en la sección de "Reservoir"
    if (in_reservoir_section && grepl(":SeepageParameters", line)) {
      # Reemplazar el valor actual de WeirCoefficient por x
      lines[i] <- paste(":SeepageParameters", y ,"102.62" ) # 102 ES LA COTA DEL ACUIFERO 
    }

  }
  
  # Escribir los cambios de vuelta en el archivo
  writeLines(lines, rvh_file_path)
  # 
  # cat("WeirCoefficient modificado a", x, "en el archivo", rvh_file_path, "\n")
}


modify_gr4j_params_from_list <- function(rvp_file_path, param_list) {
  
  
  weircoef <- param_list$reservoir$WeirCoefficient
  
  seepage <- param_list$reservoir$SeepageParameters

  
  WeirCoef(rvh_file_path_salida,weircoef, seepage)
  WeirCoef(rvh_file_path_entrada,weircoef, seepage)
  
  
  lines <- readLines(rvp_file_path)
  
  # State variables to identify relevant sections
  reading_soil <- FALSE
  reading_land_use <- FALSE
  reading_global_params <- FALSE
  
  # Variables to store the parameter names in the lists
  soil_param_names <- NULL
  land_use_param_names <- NULL
  
  # Loop through each line in the file
  for (i in seq_along(lines)) {
    line <- lines[i]
    
    # Identify the start of the SoilParameterList section
    if (grepl(":SoilParameterList", line)) {
      reading_soil <- TRUE
      # Extract the parameter names
      param_line <- lines[i + 1]
      soil_param_names <- unlist(strsplit(param_line, ","))[-1]  # Exclude ':Parameters'
      soil_param_names <- trimws(soil_param_names)
      next
    }
    
    # Identify the start of the LandUseParameterList section
    if (grepl(":LandUseParameterList", line)) {
      reading_land_use <- TRUE
      # Extract the parameter names
      param_line <- lines[i + 1]
      land_use_param_names <- unlist(strsplit(param_line, ","))[-1]  # Exclude ':Parameters'
      land_use_param_names <- trimws(land_use_param_names)
      next
    }
    
    # Identify the end of sections
    if (grepl(":EndSoilParameterList", line)) {
      reading_soil <- FALSE
    }
    
    if (grepl(":EndLandUseParameterList", line)) {
      reading_land_use <- FALSE
    }
    
    # Modify parameters in the SoilParameterList section
    if (reading_soil && grepl(",", line)) {
      parts <- unlist(strsplit(line, ","))
      name <- trimws(parts[1])
      
      # Check if the name is in the list of soil parameters
      if (name %in% names(param_list$soil_parameters)) {
        soil_params <- param_list$soil_parameters[[name]]
        
        # Loop through the parameter names and update if applicable
        for (param_name in names(soil_params)) {
          param_index <- which(soil_param_names == param_name) + 1  # +1 for the soil name
          if (length(param_index) > 0 && param_index <= length(parts)) {
            parts[param_index] <- as.character(soil_params[[param_name]])
          }
        }
        # Update the line in the lines vector
        lines[i] <- paste(parts, collapse = ",")
      }
    }
    
    # Modify parameters in the LandUseParameterList section
    if (reading_land_use && grepl(",", line)) {
      parts <- unlist(strsplit(line, ","))
      name <- trimws(parts[1])
      
      # Check if the name is in the list of land use parameters
      if (name %in% names(param_list$land_use_parameters)) {
        land_use_params <- param_list$land_use_parameters[[name]]
        
        # Loop through the parameter names and update if applicable
        for (param_name in names(land_use_params)) {
          param_index <- which(land_use_param_names == param_name) + 1  # +1 for the land use name
          if (length(param_index) > 0 && param_index <= length(parts)) {
            parts[param_index] <- as.character(land_use_params[[param_name]])
          }
        }
        # Update the line in the lines vector
        lines[i] <- paste(parts, collapse = ",")
      }
      
      # Modify MELT_FACTOR and DD_MELT_TEMP for 'Ice1'
      if (name == "Ice1" && "melt_factor_parameters" %in% names(param_list)) {
        melt_factor_params <- param_list$melt_factor_parameters[[name]]
        
        # Update MELT_FACTOR
        param_index_melt <- which(land_use_param_names == "MELT_FACTOR") + 1
        if (length(param_index_melt) > 0 && param_index_melt <= length(parts)) {
          parts[param_index_melt] <- as.character(melt_factor_params["MELT_FACTOR"])
        }
        
        # Update DD_MELT_TEMP
        param_index_dd_melt <- which(land_use_param_names == "DD_MELT_TEMP") + 1
        if (length(param_index_dd_melt) > 0 && param_index_dd_melt <= length(parts)) {
          parts[param_index_dd_melt] <- as.character(melt_factor_params["DD_MELT_TEMP"])
        }
        
        # Update the line in the lines vector
        lines[i] <- paste(parts, collapse = ",")
      }
      
      # Repeat for 'Ice2'
      if (name == "Ice2" && "melt_factor_parameters" %in% names(param_list)) {
        melt_factor_params <- param_list$melt_factor_parameters[[name]]
        
        # Update MELT_FACTOR
        param_index_melt <- which(land_use_param_names == "MELT_FACTOR") + 1
        if (length(param_index_melt) > 0 && param_index_melt <= length(parts)) {
          parts[param_index_melt] <- as.character(melt_factor_params["MELT_FACTOR"])
        }
        
        # Update DD_MELT_TEMP
        param_index_dd_melt <- which(land_use_param_names == "DD_MELT_TEMP") + 1
        if (length(param_index_dd_melt) > 0 && param_index_dd_melt <= length(parts)) {
          parts[param_index_dd_melt] <- as.character(melt_factor_params["DD_MELT_TEMP"])
        }
        
        # Update the line in the lines vector
        lines[i] <- paste(parts, collapse = ",")
      }
      
      # Repeat for 'Ice3'
      if (name == "Ice3" && "melt_factor_parameters" %in% names(param_list)) {
        melt_factor_params <- param_list$melt_factor_parameters[[name]]
        
        # Update MELT_FACTOR
        param_index_melt <- which(land_use_param_names == "MELT_FACTOR") + 1
        if (length(param_index_melt) > 0 && param_index_melt <= length(parts)) {
          parts[param_index_melt] <- as.character(melt_factor_params["MELT_FACTOR"])
        }
        
        # Update DD_MELT_TEMP
        param_index_dd_melt <- which(land_use_param_names == "DD_MELT_TEMP") + 1
        if (length(param_index_dd_melt) > 0 && param_index_dd_melt <= length(parts)) {
          parts[param_index_dd_melt] <- as.character(melt_factor_params["DD_MELT_TEMP"])
        }
        
        # Update the line in the lines vector
        lines[i] <- paste(parts, collapse = ",")
      }
    }
    
    # Modify global parameters
    if (grepl(":GlobalParameter", line)) {
      # Extract the parameter name and value
      param_parts <- unlist(strsplit(line, "\\s+"))
      if (length(param_parts) >= 2) {
        param_name <- param_parts[2]
        if (param_name %in% names(param_list$global_parameters)) {
          new_value <- as.character(param_list$global_parameters[[param_name]])
          # Reconstruct the line with the new value
          lines[i] <- paste(":GlobalParameter", param_name, new_value)
        }
      }
    }
  }
  
  writeLines(lines, rvp_file_path)
  # cat("Parameters modified and saved to", rvp_file_path, "\n")
}

generate_param_list <- function(x) {
  param_list <- list(
    soil_parameters = list(
      # Perfiles de suelo (30 en total)
      "Aporte_Lateral_0_1" = c(POROSITY = x[79], GR4J_X2 = x[1], GR4J_X3 = x[40]),
      "Aporte_Lateral_1_2" = c(POROSITY = x[80], GR4J_X2 = x[2], GR4J_X3 = x[41]),
      "Aporte_Quilque_0_1" = c(POROSITY = x[81], GR4J_X2 = x[3], GR4J_X3 = x[42]),
      "Aporte_Quilque_1_2" = c(POROSITY = x[82], GR4J_X2 = x[4], GR4J_X3 = x[43]),
      "Pie_Villarrica_0_1" = c(POROSITY = x[83], GR4J_X2 = x[5], GR4J_X3 = x[44]),
      "Pie_Villarrica_1_2" = c(POROSITY = x[84], GR4J_X2 = x[6], GR4J_X3 = x[45]),
      "Punta_Villarrica_1_0_1" = c(POROSITY = x[85], GR4J_X2 = x[7], GR4J_X3 = x[46]),
      "Punta_Villarrica_1_1_2" = c(POROSITY = x[86], GR4J_X2 = x[8], GR4J_X3 = x[47]),
      "Punta_Villarrica_2_0_1" = c(POROSITY = x[87], GR4J_X2 = x[9], GR4J_X3 = x[48]),
      "Punta_Villarrica_2_1_2" = c(POROSITY = x[88], GR4J_X2 = x[10], GR4J_X3 = x[49]),
      "Punta_Villarrica_3_0_1" = c(POROSITY = x[89], GR4J_X2 = x[11], GR4J_X3 = x[50]),
      "Punta_Villarrica_3_1_2" = c(POROSITY = x[90], GR4J_X2 = x[12], GR4J_X3 = x[51]),
      "Aporte_Central_Quelhue_0_1" = c(POROSITY = x[91], GR4J_X2 = x[13], GR4J_X3 = x[52]),
      "Aporte_Central_Quelhue_1_2" = c(POROSITY = x[92], GR4J_X2 = x[14], GR4J_X3 = x[53]),
      "Aporte_Reserva_1_2" = c(POROSITY = x[93], GR4J_X2 = x[15], GR4J_X3 = x[54]),
      "Pie_Villarrica_1_0_1" = c(POROSITY = x[94], GR4J_X2 = x[16], GR4J_X3 = x[55]),
      "Pie_Villarrica_1_1_2" = c(POROSITY = x[95], GR4J_X2 = x[17], GR4J_X3 = x[56]),
      "Punta_Villarrica_1_1_0_1" = c(POROSITY = x[96], GR4J_X2 = x[18], GR4J_X3 = x[57]),
      "Punta_Villarrica_1_1_1_2" = c(POROSITY = x[97], GR4J_X2 = x[19], GR4J_X3 = x[58]),
      "Punta_Villarrica_1_2_0_1" = c(POROSITY = x[98], GR4J_X2 = x[20], GR4J_X3 = x[59]),
      "Punta_Villarrica_1_2_1_2" = c(POROSITY = x[99], GR4J_X2 = x[21], GR4J_X3 = x[60]),
      "Punta_Villarrica_1_3_0_1" = c(POROSITY = x[100], GR4J_X2 = x[22], GR4J_X3 = x[61]),
      "Punta_Villarrica_1_3_1_2" = c(POROSITY = x[101], GR4J_X2 = x[23], GR4J_X3 = x[62]),
      "Aporte_Trancura_0_1" = c(POROSITY = x[102], GR4J_X2 = x[24], GR4J_X3 = x[63]),
      "Aporte_Trancura_1_2" = c(POROSITY = x[103], GR4J_X2 = x[25], GR4J_X3 = x[64]),
      "Aporte_Central_Liucura_0_1" = c(POROSITY = x[104], GR4J_X2 = x[26], GR4J_X3 = x[65]),
      "Aporte_Central_Liucura_1_2" = c(POROSITY = x[105], GR4J_X2 = x[27], GR4J_X3 = x[66]),
      "Aporte_Lateral_Liucura_0_1" = c(POROSITY = x[106], GR4J_X2 = x[28], GR4J_X3 = x[67]),
      "Aporte_Lateral_Liucura_1_2" = c(POROSITY = x[107], GR4J_X2 = x[29], GR4J_X3 = x[68]),
      "Aporte_Central_Carhuello_0_1" = c(POROSITY = x[108], GR4J_X2 = x[30], GR4J_X3 = x[69]),
      "Aporte_Central_Carhuello_1_2" = c(POROSITY = x[109], GR4J_X2 = x[31], GR4J_X3 = x[70]),
      "Aporte_Lateral_Carhuello_0_1" = c(POROSITY = x[110], GR4J_X2 = x[32], GR4J_X3 = x[71]),
      "Aporte_Lateral_Carhuello_1_2" = c(POROSITY = x[111], GR4J_X2 = x[33], GR4J_X3 = x[72]),
      "Aporte_Central_Caburgua_0_1" = c(POROSITY = x[112], GR4J_X2 = x[34], GR4J_X3 = x[73]),
      "Aporte_Central_Caburgua_1_2" = c(POROSITY = x[113], GR4J_X2 = x[35], GR4J_X3 = x[74]),
      "Aporte_Lateral_Caburgua_0_1" = c(POROSITY = x[114], GR4J_X2 = x[36], GR4J_X3 = x[75]),
      "Aporte_Lateral_Caburgua_1_2" = c(POROSITY = x[115], GR4J_X2 = x[37], GR4J_X3 = x[76]),
      # Nuevos perfiles agregados
      "aquifer" = c(POROSITY = x[116], GR4J_X2 = x[38], GR4J_X3 = x[77]),
      "temp" = c(POROSITY = x[117], GR4J_X2 = x[39], GR4J_X3 = x[78])
    ),
    land_use_parameters = list(
      "Forest" = c(GR4J_X4 = x[118]),
      "Forest_Ice" = c(GR4J_X4 = x[119]),
      "Grassland" = c(GR4J_X4 = x[120]),
      "Ice1" = c(GR4J_X4 = x[121]),
      "Ice2" = c(GR4J_X4 = x[122]),
      "Ice3" = c(GR4J_X4 = x[123]),
      "OPEN" = c(GR4J_X4 = x[124])
    ),
    melt_factor_parameters = list(
      "Ice1" = c(MELT_FACTOR = x[127], DD_MELT_TEMP = x[131]),
      "Ice2" = c(MELT_FACTOR = x[128], DD_MELT_TEMP = x[132]),
      "Ice3" = c(MELT_FACTOR = x[129], DD_MELT_TEMP = x[133])
    ),
    global_parameters = list(
      "AIRSNOW_COEFF" = x[125],
      "AVG_ANNUAL_SNOW" = x[126],
      "RAINSNOW_TEMP" = x[130]
    ),
    reservoir = list(
      "WeirCoefficient" = x[134],
      "SeepageParameters" = x[135]
    )
    
  )
  return(param_list)
}


modificar_y_ejecutar_raven <- function(param_list, rvp_file_path,nombre_base_rvi,resultados_dir,raven_executable) {
  # Definir las rutas y archivos específicos dentro de la función
  modify_gr4j_params_from_list(rvp_file_path, param_list)
  
  # Ejecutar el modelo Raven con el archivo RVP modificado
  resultado <- ejecutar_raven(raven_executable, resultados_dir, nombre_base_rvi)
  
  return(resultado)
}

obtener_kge_nse <- function(ruta_diagnosticos, indice) {
  diagnosticos <- read.csv(ruta_diagnosticos)
  kge <- diagnosticos$DIAG_KLING_GUPTA[indice]
  nse <- diagnosticos$DIAG_NASH_SUTCLIFFE[indice]
  return(list(KGE = kge, NSE = nse))
}

calcular_kge_con_parametros <- function(param_list, rvp_file_path, raven_executable, resultados_dir, nombre_base_rvi, nombres_archivos, indice) {
  eliminar_archivos(resultados_dir, nombres_archivos)
  
  resultado_modelo <- modificar_y_ejecutar_raven(param_list, rvp_file_path, nombre_base_rvi, resultados_dir, raven_executable)
  ruta_diagnosticos <- file.path(resultados_dir, "Diagnostics.csv")
  resutados_all <- obtener_kge_nse(ruta_diagnosticos, indice)
  KGE <- resutados_all$KGE
  print(KGE)
  return(KGE)
  
}

evaluar_modelos <- function(x) {
  param_list = generate_param_list(x)
  
  kge_salida <- calcular_kge_con_parametros(param_list, rvp_file_path_salida, raven_executable_salida, resultados_dir_salida, nombre_base_rvi, nombres_archivos, 3)
  kge_entrada <- calcular_kge_con_parametros(param_list, rvp_file_path_entrada, raven_executable_entrada, resultados_dir_entrada, nombre_base_rvi, nombres_archivos, 4)
  
  return(c(-kge_entrada, -kge_salida))
}




#### CALIBRAR MODELO R ####


param_limits <- list(
  GR4J_X2_min = -25, GR4J_X2_max = 25,            # Límites para GR4J_X2: P
  GR4J_X3_min = 0.1, GR4J_X3_max = 600,          # Límites para GR4J_X3: no P
  GR4J_X4_min = 1, GR4J_X4_max = 100,              # Límites para GR4J_X4: RAVEN
  POROSITY_min = 0.1, POROSITY_max = 0.6,         # Límites para POROSITY
  AIRSNOW_COEFF_min = 0, AIRSNOW_COEFF_max = 0.1, # Límites para AIRSNOW_COEFF : CEMANEIGE 
  AVG_ANNUAL_SNOW_min = 0, AVG_ANNUAL_SNOW_max = 100, # Límites para AVG_ANNUAL_SNOW: CEMANEIGE
  MELT_FACTOR_min = 2, MELT_FACTOR_max = 10,#  Límites para MELT_FACTOR: CEMANEIGE
  RAINSNOW_TEMP_min =0, RAINSNOW_TEMP_max = 0, # ESTOS PARAMETROS SE LES DA UN MISMO LIMITE PARA QUE NO VARIEN Y FINALMENTE NO HAGAN NADA EN EL MODELO Y EN LA CALIBRACION
  DD_MELT_TEMP_min =0, DD_MELT_TEMP_max =0,
  weir_min = 0.5, weir_max = 1,
  seepage_min = 0.0005 , seepage_max = 0.5
) 

# Número total de parámetros, considerando los 6 adicionales:
num_params <- 135  # 



lowerBounds <- c(
  rep(param_limits$GR4J_X2_min, 39),        # GR4J_X2
  rep(param_limits$GR4J_X3_min, 39),        # GR4J_X3
  rep(param_limits$POROSITY_min, 39),       # POROSITY
  rep(param_limits$GR4J_X4_min, 7),         # GR4J_X4
  param_limits$AIRSNOW_COEFF_min,           # AIRSNOW_COEFF
  param_limits$AVG_ANNUAL_SNOW_min,         # AVG_ANNUAL_SNOW                                
  rep(param_limits$MELT_FACTOR_min, 3),             # MELT_FACTOR  rep(param_limits$MELT_FACTOR_min, 4), 
  param_limits$RAINSNOW_TEMP_min,
  rep(param_limits$DD_MELT_TEMP_min, 3) ,
  param_limits$weir_min,
  param_limits$seepage_min
)


upperBounds <- c(
  rep(param_limits$GR4J_X2_max, 39),        # GR4J_X2
  rep(param_limits$GR4J_X3_max, 39),        # GR4J_X3
  rep(param_limits$POROSITY_max, 39),       # POROSITY
  rep(param_limits$GR4J_X4_max, 7),         # GR4J_X4
  param_limits$AIRSNOW_COEFF_max,           # AIRSNOW_COEFF
  param_limits$AVG_ANNUAL_SNOW_max,         # AVG_ANNUAL_SNOW
  rep(param_limits$MELT_FACTOR_max, 3),             # MELT_FACTOR  rep(param_limits$MELT_FACTOR_min, 4), 
  param_limits$RAINSNOW_TEMP_max,
  rep(param_limits$DD_MELT_TEMP_max, 3),
  param_limits$weir_max,
  param_limits$seepage_max
  
)

cat("Límites inferiores:\n")
print(lowerBounds)
cat("Límites superiores:\n")
print(upperBounds)


resultado_nsga2_R <- nsga2R(
  fn = evaluar_modelos,
  varNo = num_params,
  objDim = 2,
  lowerBounds = lowerBounds,
  upperBounds = upperBounds,
  popSize = 135,  # Tamaño de la población
  generations = 100,  # Número de generaciones para obtener resultados significativos
  cprob = 0.9,  # Probabilidad de cruce
  XoverDistIdx = 20,  # Índice de distribución de cruce
  mprob = 0.2,  # Probabilidad de mutación
  MuDistIdx = 20  # Índice de distribución de mutación
)


par(mfrow = c(1,1))

# Imprimir los resultados de la optimización
print(resultado_nsga2_R)

# Graficar los resultados de la optimización
plot(-resultado_nsga2_R$objectives)




#### GUARDAR RESULTADOS ####


directorio_guardado <- ".../Memoria/RAVEN1/01_resultados_calib"
nombre_archivo <- "resultado_nsga2_Rseepage_100_def_cf111.rds"

# Guardar el objeto resultado_nsga2 en el archivo .rds
saveRDS(resultado_nsga2_R, file = file.path(directorio_guardado, nombre_archivo))

cat("El objeto resultado_nsga2 se ha guardado en:", file.path(directorio_guardado, nombre_archivo))


# resultado_nsga2_R$parameters[1,235]
# 














