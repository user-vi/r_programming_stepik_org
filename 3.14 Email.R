library(dplyr)

####  отправка емейл - вариант 1

library(emayili)
email <- envelope() %>%
  from("") %>%
  to("") %>%
  subject(paste("Прогнозная модель по состоянию на", Sys.Date())) %>% 
  text("Прогноз") %>%
  attachment("reg.txt")

smtp <- emayili::server(host = "smtp.gmail.com",
                        port = 465,
                        # username = "bob@gmail.com",
                        # password = "bd40ef6d4a9413de9c1318a65cbae5d7"
                        username = "",
                        password = ""
)

smtp(email, verbose = TRUE)

print(email, details = TRUE)

####  отправка емейл - вариант 2
# получить сервисный ключ, не OAuth
# включить api почты у аккаунта

devtools::install_github("r-lib/gmailr")
suppressPackageStartupMessages(library(gmailr))
library(jsonlite)
use_secret_file("credentials.json")

gm_auth_configure(path = "credentials.json")
gm_auth()
# gm_auth_configure()

test_email <-
  gm_mime() %>%
  gm_to("linux5404@gmail.com") %>%
  gm_from("linux5404@gmail.com") %>%
  gm_subject("this is just a gmailr test") %>%
  gm_text_body("Can you hear me now?") %>% 
  gm_attach_file("reg.txt")

# Verify it looks correct
gm_create_draft(test_email)

# If all is good with your draft, then you can send it
gm_send_message(test_email)