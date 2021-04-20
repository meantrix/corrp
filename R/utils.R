#Load All Parent Variables to Child Function
load_vars <- function(){
  variables <- ls(envir = parent.frame(2))

  for (var in variables) {
    assign(var, get(var, envir = parent.frame(2)),envir = parent.frame(1))
  }
}

list_corrP = function(){
  ls("package::corrP")
}

