serverParse <- function(str,con){
  cmdlist <- strsplit(str," ")
  cmd <- cmdlist[[1]][1]
  # if cmd is exit, return -1 to signal server to quit
  if (cmd == "exit"){
    return(-1)
  }
  if (cmd == "ol"){
    serialize(dataframe, con)
    return(0)
  }
  if (cmd == "get"){
    arg <- cmdlist[[1]][2]
    obj_name = arg
    obj = get(arg)
    attributes(obj)$name = obj_name
    serialize(obj,con)
    return(0)
  }
  if (cmd == "put"){
    arg <- cmdlist[[1]][2]
    # get the object
    obj <- unserialize(con)
    # get object's name
    obj_name <- attributes(obj)$name
    # clean obj's name attribute
    attributes(obj)$name <- NULL
    # assign object to its name
    assign(obj_name, obj, envir = .GlobalEnv)
    # update the object table
    size <- as.numeric(object.size(obj))
    obj_class <- class(obj)
    obj_df <- data.frame("ObjectName"=obj_name, "NBytes"=size, "ObjectClass"=obj_class)
    dataframe <<- rbind(dataframe,obj_df)
    return(0)
  }
}

clientParse <- function(str,con){
  cmdlist <- strsplit(str," ")
  cmd <- cmdlist[[1]][1]
  # if cmd is exit, return -1 to signal server to quit
  if (cmd == "exit"){
    return(-1)
  }
  if (cmd == "ol"){
    print(unserialize(con))
    return(0)
  }
  if (cmd == "get"){
    # get the object
    obj <- unserialize(con)
    # get object's name
    obj_name <- attributes(obj)$name
    # clean obj's name attribute
    attributes(obj)$name <- NULL
    # assign object to its name
    assign(obj_name, obj, envir = .GlobalEnv)
    return(0)
  }
  if (cmd == "put"){
    arg <- cmdlist[[1]][2]
    obj_name <- arg
    obj = get(obj_name)
    attributes(obj)$name = obj_name
    serialize(obj,con)
    return(0)
  }
}

otpServer <- function(nclnt, port){
  # construct dataframe for 'ol' command
  if (!exists("dataframe")){
    dataframe <<- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("ObjectName","NBytes", "ObjectClass"))
  }
  # accept connections from clients
  clist=vector('list',nclnt)
  for(i in 1:nclnt) clist[i] <- socketConnection(host="", port = port, blocking=TRUE,server=TRUE, open="r+b")
  for (i in 1:nclnt){
    print(paste0("This is client number ", i))
    con=getConnection(clist[[i]])
    while(TRUE){
      cmd <- readLines(con,1) # why 1 not -1
      stats <- serverParse(cmd,con)
      if (stats == -1){
        break
      }
    }
    close(con)
  }
}

otpClient <- function(host,port){
  con <- socketConnection(host = host, port = port, blocking = TRUE, server = FALSE, open = "r+b")
  while(TRUE){
    
    f <- file("stdin")
    open(f)
    cat("#otp ")
    # get command from stdin
    sendme <- readLines(f, n=1)
    # send command to server to prepare
    writeLines(sendme, con)
    stats <- clientParse(sendme, con)
    if (stats == -1) break
  }
  close(con)
}