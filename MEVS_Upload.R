#MEVS Upload
  print("MEVS_Upload")
  
    
    if(exists(c("FORMULA_DEF","MKT_EI_DEF","MKT_EI_INPUT_SET","MKT_REFERENCE_CURVE")) != TRUE){
      
      FORMULA_DEF = fread(paste0(path_in,"/FORMULA_DEF.CSV"))
      MKT_EI_DEF = fread(paste0(path_in,"/MKT_EI_DEF.CSV"))
      FORMULA_DEF = fread(paste0(path_in,"/FORMULA_DEF.CSV"))
      MKT_EI_INPUT = fread(paste0(path_in,"/MKT_EI_INPUT.CSV"))
      FORMULA_DEF = fread(paste0(path_in,"/FORMULA_DEF.CSV"))
      MKT_EI_DEF = fread(paste0(path_in,"/MKT_EI_DEF.CSV"))
      
    }#Subirá los Csv's si no están cargados o no se ha usado Mevs_Format.R

  
  #MEVS_NAME
  MEVS_NAME_pure = readline(prompt="Introduce Pure Mevs NAME: ")
  
  Cond = readline(prompt="Delay (1) or Advance (2)?: ")
  if(Cond == 1){
    DELAY = readline(prompt="Introduce Delay, _(Number):")
    MEVS_NAME_final = paste0(MEVS_NAME_pure,"_",DELAY)
    print("Nombre Mev:")
    print(MEVS_NAME_final)
  }else if(Cond == 2){
    ADVANCE = readline(prompt="Introduce Advance, _A(Number):")
    MEVS_NAME_final = paste0(MEVS_NAME_pure,"_A",ADVANCE)
    print("Nombre Mev:")
    print(MEVS_NAME_final)
  }
  
  print = MEVS_NAME_final
  
  #SCENARIO
  print("Scenarios disponibles:")
  print(MKT_EI_INPUT_SET$MKT_EI_INPUT_SET)
  SCENARIO = readline(prompt="Introduce SCENARIO: ")
  
  print = cbind(MEVS_NAME_final,SCENARIO)
  print(print)

  #FECHAS
  option_dates = readline(prompt="Para las fechas, cargar CSV con fechas (1), introducir en intervalo de fechas (2): ")
  
  if(option_dates == 1){
    FECHAS = fread(paste0(path_out,"FECHAS.CSV"))
    FECHAS = unique(FECHAS)
    FECHA_INI = min(FECHAS$FECHAS)
    FECHA_FIN = max(FECHAS$FECHAS)
  }else if(option_dates == 2){
    FECHAS = unique(MKT_EI_INPUT$V4)
    FECHA_INI = readline(prompt="Introduce valor fecha inicial (DD-MM-YYYY): ")
    FECHA_FIN = readline(prompt="Introduce valor fecha final (DD-MM-YYYY): ")
    # FECHA_INI =  "30-06-2018"
    # FECHA_FIN =  "31-12-2023"

    FECHAS = FECHAS[which(substr(FECHAS,4,5) >= substr(FECHA_INI, 4,5) &
                            substr(FECHAS,7,10) >= substr(FECHA_INI, 7,10) &
                           
                            substr(FECHAS,4,5) <= substr(FECHA_FIN, 4,5) &
                            substr(FECHAS,7,10) <= substr(FECHA_FIN, 7,10))]
    FECHAS = as.data.table(FECHAS)
    
  }
    print = cbind(MEVS_NAME_final,SCENARIO, FECHA_INI, FECHA_FIN)
    print(print) 
    print("--------------------------")
  
  #########################
  #OPERACIONES FORMULA_DEF#
  #########################        
  add_formula_def = FORMULA_DEF
  add_formula_def = add_formula_def[1,][1]
  cond_mev = FORMULA_DEF[which(FORMULA_DEF[,2] == MEVS_NAME_final),]
  if(nrow(cond_mev) != 0){
    print("Esta variable existe, no se añadirá")
  }else if(nrow(cond_mev) == 0){
    print("Añadiendo variable transformada:")
    #Condicion formula
    #Delay
    if(Cond == 1){
      if(DELAY == 1){
        formula =  paste0('eco_index_value(""',MEVS_NAME_pure,'"",vd-tenor(m:3))')
      }else if(DELAY == 2){
        formula =  paste0('eco_index_value(""',MEVS_NAME_pure,'"",vd-tenor(m:6))')
      }else if(DELAY == 3){
        formula =  paste0('eco_index_value(""',MEVS_NAME_pure,'"",vd-tenor(m:9))')
      }else if(DELAY == 4){
        formula =  paste0('eco_index_value(""',MEVS_NAME_pure,'"",vd-tenor(m:12))')
      }
      #Advance
    }else if(Cond == 2){
      if(ADVANCE == 1){
        formula =  paste0('eco_index_value(""',MEVS_NAME_pure,'"",vd+tenor(m:3))')
      }else if(ADVANCE == 2){
        formula =  paste0('eco_index_value(""',MEVS_NAME_pure,'"",vd+tenor(m:6))')
      }else if(ADVANCE == 3){
        formula =  paste0('eco_index_value(""',MEVS_NAME_pure,'"",vd+tenor(m:9))')
      }else if(ADVANCE == 4){
        formula =  paste0('eco_index_value(""',MEVS_NAME_pure,'"",vd+tenor(m:12))')
      }
    }#fin de advance/delay
    #Condicion id
    max_id = max(as.vector(FORMULA_DEF[[1]]))
    id = max_id+1
    #Reemplazo
    add_formula_def[1,1][1] = id
    add_formula_def[1,2][1] = MEVS_NAME_final
    add_formula_def[1,7][1] = formula
    #Union de fila
    FORMULA_DEF = rbind(FORMULA_DEF, add_formula_def)
    #Guardado
    print("--------------------------")
    print("FORMULA_DEF terminado.")
    print("--------------------------")
    fwrite(FORMULA_DEF, paste0(path_out,folder_out,"/FORMULA_DEF.CSV"), col.names = F)
  }#fin añadir variables
  
  
  ########################
  #OPERACIONES MKT_EI_DEF#
  ########################
  cond_mev_pure = MKT_EI_DEF[which(MKT_EI_DEF[,1] == MEVS_NAME_pure),]
  cond_mev = MKT_EI_DEF[which(MKT_EI_DEF[,1] == MEVS_NAME_final),]
  fila = MKT_EI_DEF[1,][1]
  #Condicion variable pura
  if(nrow(cond_mev_pure) != 0){
    print("Esta variable existe, no se añadirá")
  }else if(nrow(cond_mev_pure) == 0){
    print("Añadiendo variable pura:")
    add_MKT_EI_DEF_pure = fila
    add_MKT_EI_DEF_pure[1,1] = MEVS_NAME_pure
    add_MKT_EI_DEF_pure[1,2] = "I"
    add_MKT_EI_DEF_pure[1,9] = NA
    MKT_EI_DEF = rbind(MKT_EI_DEF, add_MKT_EI_DEF_pure)
  }
  #Condicion variable transformada
  if(nrow(cond_mev) != 0){
    print("Esta variable existe, no se añadirá")
  }else if(nrow(cond_mev) == 0){
    print("Añadiendo variable transformada:")
    add_MKT_EI_DEF_final = fila
    add_MKT_EI_DEF_final[1,1] = MEVS_NAME_final
    add_MKT_EI_DEF_final[1,2] = "F"
    add_MKT_EI_DEF_final[1,9] = id
    MKT_EI_DEF = rbind(MKT_EI_DEF, add_MKT_EI_DEF_final)
  }
  #Guardado MKT_EI_DEF
  print("--------------------------")
  print("MKT_EI_DEF terminado.")
  print("--------------------------")
  fwrite(MKT_EI_DEF, paste0(path_out,folder_out,"/MKT_EI_DEF.CSV"), col.names = F)
  
  
  ##########################
  #OPERACIONES MKT_EI_INPUT#
  ##########################
  #Aqui se escriben todas, las puras y las transformadas
  #Pura
  print("Añadiendo filas variable pura a MKT_EI_INPUT...")
  FECHAS$V4 = FECHAS$FECHAS
  # FECHAS$V4 = FECHAS
  FECHAS$V2 = MEVS_NAME_pure
  FECHAS$V3 = SCENARIO
  FECHAS$V5 = NA
  FECHAS$V6 = sample(0.00000000:20.23532012, length(FECHAS$V4), replace=TRUE)

  FECHAS_pure=FECHAS[, c("V2", "V3", "V4", "V5", "V6")]
  
  MKT_EI_INPUT = rbind(MKT_EI_INPUT, FECHAS_pure)
  
  
  #Transformada
  print("Añadiendo filas variable transformada a MKT_EI_INPUT...")
  FECHAS$V4 = FECHAS$FECHAS
  # FECHAS$V4 = FECHAS
  FECHAS$V2 = MEVS_NAME_final
  FECHAS$V3 = SCENARIO
  FECHAS$V5 = NA
  FECHAS$V6 = sample(0.00000000:20.23532012, length(FECHAS$V4), replace=TRUE)
  
  FECHAS_final=FECHAS[, c("V2", "V3", "V4", "V5", "V6")]
  
  MKT_EI_INPUT = rbind(MKT_EI_INPUT, FECHAS_final)
   
  print("--------------------------")
  print("MKT_EI_INPUT terminado")
  print("--------------------------")
  fwrite(MKT_EI_INPUT, paste0(path_out,folder_out,"/MKT_EI_INPUT.CSV"), col.names = F)
  
  
  ##############################
  #OPERACIONES MKT_EI_INPUT_SET#
  ##############################
  add = MKT_EI_INPUT_SET
  add = add[1]
  for(s in 1:dim(MKT_EI_INPUT_SET)[1]){
    if(MKT_EI_INPUT_SET[1,1][1] != SCENARIO){
      print("Añadiendo SCENARIO a MKT_EI_INPUT_SET...")
      add[1,1] = SCENARIO
      MKT_EI_INPUT_SET = rbind(MKT_EI_INPUT_SET, add)
    }
  }
  print("--------------------------")
  print("MKT_EI_INPUT_SET terminado")
  print("--------------------------")
  fwrite(MKT_EI_INPUT_SET, paste0(path_out,folder_out,"/MKT_EI_INPUT_SET.CSV"), col.names = F)
  
  
  #################################
  #OPERACIONES MKT_REFERENCE_CURVE#
  #################################
  fila = MKT_REFERENCE_CURVE[1,][1]
  cond_mev_pure = MKT_REFERENCE_CURVE[which(MKT_REFERENCE_CURVE[,1] == MEVS_NAME_pure),]
  cond_mev_final = MKT_REFERENCE_CURVE[which(MKT_REFERENCE_CURVE[,1] == MEVS_NAME_final),]
  
  if(nrow(cond_mev_pure) != 0){
    print("Esta variable existe, no se añadirá")
  }else if(nrow(cond_mev_pure) == 0){
    print("Añadiendo variable pura:")
    add_MKT_REFERENCE_CURVE_pure = fila
    add_MKT_REFERENCE_CURVE_pure[1,1] = MEVS_NAME_pure
    MKT_REFERENCE_CURVE = rbind(MKT_REFERENCE_CURVE, add_MKT_REFERENCE_CURVE_pure)
  }
  if(nrow(cond_mev_final) != 0){
    print("Esta variable existe, no se añadirá")
  }else if(nrow(cond_mev_final) == 0){
    print("Añadiendo variable transformada:")
    add_MKT_REFERENCE_CURVE_final = fila
    add_MKT_REFERENCE_CURVE_final[1,1] = MEVS_NAME_final
    MKT_REFERENCE_CURVE = rbind(MKT_REFERENCE_CURVE, add_MKT_REFERENCE_CURVE_final)
  }
  print("--------------------------")
  print("MKT_REFERENCE_CURVE terminado")
  print("--------------------------")
  fwrite(MKT_REFERENCE_CURVE, paste0(path_out,folder_out,"/MKT_REFERENCE_CURVE.CSV"), col.names = F)
  

