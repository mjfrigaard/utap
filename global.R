install.packages('rsconnect')
pkgs <- pkgs <- rsconnect::appDependencies()$Package
install.packages(pkgs)
