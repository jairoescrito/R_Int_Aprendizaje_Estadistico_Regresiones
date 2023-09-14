# Configuración y Conexión con Github


# Activar Git
library(usethis)
use_git()




create_github_token()


'
Inicializar un repositorio de Git
'
install.packages("credentials")
library(credentials)
set_github_pat()
"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"
'
Modificar credenciales
'
install.packages("gitcreds")
library(gitcreds)
gitcreds_set()

'
Conectar con repositorio
'
use_git()
use_github()
usethis::use_github()

install.packages("reticulate")