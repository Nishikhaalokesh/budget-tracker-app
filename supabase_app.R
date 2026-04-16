library(httr2)
library(jsonlite)
SUPABASE_URL <- "https://xulmdwvkxqhtzaqrtzkr.supabase.co"
SUPABASE_KEY <- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6Inh1bG1kd3ZreHFodHphcXJ0emtyIiwicm9sZSI6ImFub24iLCJpYXQiOjE3NzUzNTI0OTgsImV4cCI6MjA5MDkyODQ5OH0.Quvtm1bj1S_DCbC6dmCpMUCBCaJu9clvEvke8LiEtDE"
supabase_signup_app <- function(email,password){
  tryCatch({
    resp <- request(paste0(SUPABASE_URL,"/auth/v1/signup")) %>%
      req_headers("apikey"=SUPABASE_KEY,"Content-Type"="application/json") %>%
      req_body_json(list(email=email,password=password)) %>% req_perform()
    data <- resp %>% resp_body_json()
    if(!is.null(data$access_token)) list(success=TRUE,token=data$access_token,user_id=data$user$id,email=email)
    else list(success=FALSE,message="Signup failed")
  },error=function(e) list(success=FALSE,message=as.character(e)))
}
supabase_login_app <- function(email,password){
  tryCatch({
    resp <- request(paste0(SUPABASE_URL,"/auth/v1/token?grant_type=password")) %>%
      req_headers("apikey"=SUPABASE_KEY,"Content-Type"="application/json") %>%
      req_body_json(list(email=email,password=password)) %>% req_perform()
    data <- resp %>% resp_body_json()
    if(!is.null(data$access_token)) list(success=TRUE,token=data$access_token,user_id=data$user$id,email=email)
    else list(success=FALSE,message="Invalid email or password")
  },error=function(e) list(success=FALSE,message="Invalid email or password"))
}
supabase_save_profile_app <- function(token,user_id,name,income,currency,mode){
  tryCatch({
    request(paste0(SUPABASE_URL,"/rest/v1/profiles")) %>%
      req_headers("apikey"=SUPABASE_KEY,"Authorization"=paste("Bearer",token),
        "Content-Type"="application/json","Prefer"="resolution=merge-duplicates") %>%
      req_body_json(list(id=user_id,name=name,income=income,currency=currency,mode=mode)) %>%
      req_perform()
    TRUE
  },error=function(e) FALSE)
}
supabase_load_profile_app <- function(token,user_id){
  tryCatch({
    resp <- request(paste0(SUPABASE_URL,"/rest/v1/profiles?id=eq.",user_id)) %>%
      req_headers("apikey"=SUPABASE_KEY,"Authorization"=paste("Bearer",token)) %>%
      req_perform()
    data <- resp %>% resp_body_json()
    if(length(data)>0) data[[1]] else NULL
  },error=function(e) NULL)
}
supabase_save_categories_app <- function(token,user_id,categories_df){
  tryCatch({
    request(paste0(SUPABASE_URL,"/rest/v1/categories?user_id=eq.",user_id)) %>%
      req_headers("apikey"=SUPABASE_KEY,"Authorization"=paste("Bearer",token)) %>%
      req_method("DELETE") %>% req_perform()
    rows <- lapply(1:nrow(categories_df),function(i)
      list(user_id=user_id,category=categories_df$category[i],amount=categories_df$amount[i]))
    request(paste0(SUPABASE_URL,"/rest/v1/categories")) %>%
      req_headers("apikey"=SUPABASE_KEY,"Authorization"=paste("Bearer",token),
        "Content-Type"="application/json") %>%
      req_body_json(rows) %>% req_perform()
    TRUE
  },error=function(e) FALSE)
}
supabase_load_categories_app <- function(token,user_id){
  tryCatch({
    resp <- request(paste0(SUPABASE_URL,"/rest/v1/categories?user_id=eq.",user_id)) %>%
      req_headers("apikey"=SUPABASE_KEY,"Authorization"=paste("Bearer",token)) %>%
      req_perform()
    data <- resp %>% resp_body_json()
    if(length(data)>0) tibble(
      category=sapply(data,function(x) x$category),
      amount=sapply(data,function(x) as.numeric(unlist(x$amount))))
    else tibble(category=character(),amount=numeric())
  },error=function(e) tibble(category=character(),amount=numeric()))
}

# Create a group and get invite code
supabase_create_group_app <- function(token, user_id, group_name) {
  tryCatch({
    invite_code <- paste0(sample(LETTERS, 4, replace=TRUE), collapse="")
    invite_code <- paste0(invite_code, sample(1000:9999, 1))
    resp <- request(paste0("https://xulmdwvkxqhtzaqrtzkr.supabase.co/rest/v1/groups")) %>%
      req_headers("apikey"="eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6Inh1bG1kd3ZreHFodHphcXJ0emtyIiwicm9sZSI6ImFub24iLCJpYXQiOjE3NzUzNTI0OTgsImV4cCI6MjA5MDkyODQ5OH0.Quvtm1bj1S_DCbC6dmCpMUCBCaJu9clvEvke8LiEtDE",
                  "Authorization"=paste("Bearer",token),
                  "Content-Type"="application/json",
                  "Prefer"="return=representation") %>%
      req_body_json(list(group_name=group_name,created_by=user_id,invite_code=invite_code)) %>%
      req_perform()
    data <- resp %>% resp_body_json()
    group_id <- data[[1]]$id
    # Add creator as member
    request(paste0("https://xulmdwvkxqhtzaqrtzkr.supabase.co/rest/v1/group_members")) %>%
      req_headers("apikey"="eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6Inh1bG1kd3ZreHFodHphcXJ0emtyIiwicm9sZSI6ImFub24iLCJpYXQiOjE3NzUzNTI0OTgsImV4cCI6MjA5MDkyODQ5OH0.Quvtm1bj1S_DCbC6dmCpMUCBCaJu9clvEvke8LiEtDE",
                  "Authorization"=paste("Bearer",token),
                  "Content-Type"="application/json") %>%
      req_body_json(list(group_id=group_id,user_id=user_id)) %>%
      req_perform()
    list(success=TRUE, invite_code=invite_code, group_id=group_id)
  }, error=function(e) list(success=FALSE, message=as.character(e)))
}

# Join a group using invite code
supabase_join_group_app <- function(token, user_id, invite_code) {
  tryCatch({
    # Find group by invite code
    resp <- request(paste0("https://xulmdwvkxqhtzaqrtzkr.supabase.co/rest/v1/groups?invite_code=eq.",invite_code)) %>%
      req_headers("apikey"="eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6Inh1bG1kd3ZreHFodHphcXJ0emtyIiwicm9sZSI6ImFub24iLCJpYXQiOjE3NzUzNTI0OTgsImV4cCI6MjA5MDkyODQ5OH0.Quvtm1bj1S_DCbC6dmCpMUCBCaJu9clvEvke8LiEtDE",
                  "Authorization"=paste("Bearer",token)) %>%
      req_perform()
    data <- resp %>% resp_body_json()
    if(length(data)==0) return(list(success=FALSE,message="Invalid invite code"))
    group_id <- data[[1]]$id
    group_name <- data[[1]]$group_name
    # Join group
    request(paste0("https://xulmdwvkxqhtzaqrtzkr.supabase.co/rest/v1/group_members")) %>%
      req_headers("apikey"="eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6Inh1bG1kd3ZreHFodHphcXJ0emtyIiwicm9sZSI6ImFub24iLCJpYXQiOjE3NzUzNTI0OTgsImV4cCI6MjA5MDkyODQ5OH0.Quvtm1bj1S_DCbC6dmCpMUCBCaJu9clvEvke8LiEtDE",
                  "Authorization"=paste("Bearer",token),
                  "Content-Type"="application/json") %>%
      req_body_json(list(group_id=group_id,user_id=user_id)) %>%
      req_perform()
    list(success=TRUE, group_id=group_id, group_name=group_name)
  }, error=function(e) list(success=FALSE, message=as.character(e)))
}
