#FORMAT
  print("MEVS_Format")
  
  #FORMULA_DEF----
  print("Comienza: FORMULA_DEF")
  FORMULA_DEF = fread(paste0(path_in,"/FORMULA_DEF.CSV"))
  FORMULA_DEF = FORMULA_DEF[, c("V3","V4","V5","V6","V7","V28","V32")] 
  FORMULA_DEF$V7 = sapply(FORMULA_DEF$V7, function(x) gsub('\"\"', '\"', x))
  
  #MKT_EI_DEF----
  print("Comienza: MKT_EI_DEF")
  MKT_EI_DEF = fread(paste0(path_in,"/MKT_EI_DEF.CSV"))
  MKT_EI_DEF = MKT_EI_DEF[, c("V2","V3", "V4", "V5", "V6", "V7", "V8", "V9", "V30")]
  
  #MKT_EI_INPUT----
  print("Comienza: MKT_EI_INPUT")
  MKT_EI_INPUT = fread(paste0(path_in,"/MKT_EI_INPUT.CSV"))
  MKT_EI_INPUT = MKT_EI_INPUT[, c("V2","V3","V4","V5","V6")]
  
  meses_num = c("03", "06", "09", "12")
  meses_str = c("Mar", "Jun", "Sep", "Dec")
  
  for(f in 1:dim(MKT_EI_INPUT)[1]){
    for(m in 1:length(meses_num)){
      anyo = substr(MKT_EI_INPUT[f,"V4"], 1,2)
      mes = substr(MKT_EI_INPUT[f,"V4"], 4,6)
      dia = substr(MKT_EI_INPUT[f,"V4"], 8,11)
      
      if(mes == meses_str[m]){
        MKT_EI_INPUT[f, "V4"] = paste0(anyo,"-",meses_num[m],"-",dia)
      }
    }
  }
  
  #MKT_EI_INPUT_SET----
  print("Comienza: MKT_EI_INPUT_SET")
  MKT_EI_INPUT_SET = fread(paste0(path_in,"/MKT_EI_INPUT_SET.CSV"))
  MKT_EI_INPUT_SET = MKT_EI_INPUT_SET$EI_INPUT_SET
  MKT_EI_INPUT_SET = as.data.table(MKT_EI_INPUT_SET)
  MKT_EI_INPUT_SET[, c("V1","V2")] = rep(NA, 1, dim(MKT_EI_INPUT_SET)[1])
  
  #MKT_REFERENCE_CURVE----
  print("Comienza: MKT_REFERENCE_CURVE")
  MKT_REFERENCE_CURVE = fread(paste0(path_in,"/MKT_REFERENCE_CURVE.CSV"))
  MKT_REFERENCE_CURVE = MKT_REFERENCE_CURVE[, c("V2","V3","V4")]
  
  
  #GUARDADO----
  path_out=paste0("C:/Users/",XI,"/Desktop/MEVS_FORMAT/")
  folder_name=gsub(":","_",substr(Sys.time(), 1,19))
  folder_out=paste0("MEVS_",folder_name)
  
  if (file.exists(path_out)){
    setwd(file.path(path_out, folder_out))
  } else {
    dir.create(file.path(path_out, folder_out))
    setwd(file.path(path_out, folder_out))
  }
  
  fwrite(FORMULA_DEF, paste0(path_out,folder_out,"/FORMULA_DEF.CSV"), col.names = F)
  fwrite(MKT_REFERENCE_CURVE, paste0(path_out,folder_out,"/MKT_EI_DEF.CSV"), col.names = F)
  fwrite(MKT_EI_INPUT, paste0(path_out,folder_out,"/MKT_EI_INPUT.CSV"), col.names = F)
  fwrite(MKT_EI_INPUT_SET, paste0(path_out,folder_out,"/MKT_EI_INPUT_SET.CSV"), col.names = F)
  fwrite(MKT_REFERENCE_CURVE, paste0(path_out,folder_out,"/MKT_REFERENCE_CURVE.CSV"), col.names = F)
  
  print("Guardado terminado")
  print(Sys.time())
  
  