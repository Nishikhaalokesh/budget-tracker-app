server <- function(input, output, session) {
  user_cats <- reactiveVal(tibble(category=character(),amount=numeric()))
  prof <- reactiveVal(list(name="",income=3000,currency="USD",mode="individual",categories=tibble(category=character(),amount=numeric())))
  auth <- reactiveValues(logged_in=FALSE,token=NULL,user_id=NULL,email=NULL)
  observe({if(!auth$logged_in) updateTabItems(session,"tabs","login")})
  observeEvent(input$btn_login,{
    req(input$auth_email,input$auth_password)
    result <- supabase_login_app(input$auth_email,input$auth_password)
    if(result$success){
      auth$logged_in <- TRUE
      auth$token     <- result$token
      auth$user_id   <- result$user_id
      auth$email     <- result$email
      cats <- supabase_load_categories_app(result$token,result$user_id)
      user_cats(cats)
      pv <- supabase_load_profile_app(result$token,result$user_id)
      if(!is.null(pv)){
        prof(list(name=pv$name,income=pv$income,currency=pv$currency,mode=pv$mode,categories=cats))
        if(nrow(cats)==0) updateTabItems(session,"tabs","setup")
        else updateTabItems(session,"tabs","budget")
      } else updateTabItems(session,"tabs","setup")
    } else {
      output$auth_message <- renderUI(
        div(style="background:#ffcccc;padding:10px;border-radius:6px;margin-top:10px;",
          "Invalid email or password. Please try again."))
    }
  })
  observeEvent(input$btn_signup,{
    req(input$auth_email,input$auth_password)
    if(nchar(input$auth_password)<6){
      output$auth_message <- renderUI(
        div(style="background:#ffcccc;padding:10px;border-radius:6px;margin-top:10px;",
          "Password must be at least 6 characters."))
      return()
    }
    result <- supabase_signup_app(input$auth_email,input$auth_password)
    if(result$success){
      auth$logged_in <- TRUE
      auth$token     <- result$token
      auth$user_id   <- result$user_id
      auth$email     <- result$email
      output$auth_message <- renderUI(
        div(style="background:#ccffcc;padding:10px;border-radius:6px;margin-top:10px;",
          "Account created! Please set up your profile."))
      updateTabItems(session,"tabs","setup")
    } else {
      output$auth_message <- renderUI(
        div(style="background:#ffcccc;padding:10px;border-radius:6px;margin-top:10px;",
          "Signup failed. Please try again later."))
    }
  })
  observeEvent(input$add_category,{
    req(input$new_cat_name,input$new_cat_amount)
    user_cats(bind_rows(user_cats(),
      tibble(category=trimws(input$new_cat_name),amount=as.numeric(input$new_cat_amount))))
    updateTextInput(session,"new_cat_name",value="")
    updateNumericInput(session,"new_cat_amount",value=100)
  })
  output$categories_preview <- DT::renderDataTable({
    df <- user_cats()
    if(nrow(df)==0) return(DT::datatable(
      tibble(Message="No categories yet. Add your first one above!"),
      options=list(dom="t"),rownames=FALSE))
    total <- sum(df$amount)
    bind_rows(df %>% mutate(amount=dollar(amount)),
              tibble(category="TOTAL",amount=dollar(total))) %>%
      rename(Category=category,`Monthly Amount`=amount) %>%
      DT::datatable(options=list(dom="t",pageLength=20),rownames=FALSE)
  })
  observeEvent(input$save_profile,{
    req(input$setup_name,input$setup_income)
    cats <- user_cats()
    if(nrow(cats)==0){
      output$setup_message <- renderUI(
        div(style="background:#ffcccc;padding:10px;border-radius:6px;margin-top:10px;",
          "Please add at least one expense category before saving."))
      return()
    }
    save_profile_app(name=input$setup_name,income=input$setup_income,
      currency=input$setup_currency,mode=input$setup_mode,categories_df=cats)
    if(!is.null(auth$token)){
      supabase_save_profile_app(auth$token,auth$user_id,
        input$setup_name,input$setup_income,input$setup_currency,input$setup_mode)
      supabase_save_categories_app(auth$token,auth$user_id,cats)
    }
    prof(list(name=input$setup_name,income=as.numeric(input$setup_income),
              currency=input$setup_currency,mode=input$setup_mode,categories=cats))
    output$setup_message <- renderUI(
      div(style="background:#ccffcc;padding:10px;border-radius:6px;margin-top:10px;",
        paste0("Profile saved! Welcome, ",input$setup_name,"!")))
    updateTabItems(session,"tabs","budget")
  })
  output$profile_banner <- renderUI({
    pv <- prof()
    nm <- tryCatch(as.character(pv$name[1]),error=function(e) "")
    if(is.null(nm)||nm==""|nm=="Person"|nm=="Demo User") return(NULL)
    div(style="background:#e3f2fd;padding:10px;border-radius:6px;margin-bottom:10px;",
      paste0("Welcome back, ",nm,"!  Income: ",pv$currency," ",pv$income))
  })
  output$budget_income_display <- renderUI({
    pv  <- prof()
    nm  <- tryCatch(as.character(pv$name[1]),error=function(e) "Not set")
    inc <- tryCatch(as.character(pv$income[1]),error=function(e) "0")
    cur <- tryCatch(as.character(pv$currency[1]),error=function(e) "USD")
    mod <- tryCatch(as.character(pv$mode[1]),error=function(e) "individual")
    div(
      tags$p(tags$b("Name: "),nm),
      tags$p(tags$b("Income: "),paste0(cur," ",inc)),
      tags$p(tags$b("Mode: "),mod),
      tags$p(style="color:#888;font-size:12px;","Go to Setup Profile to update."))
  })
  budget_result <- eventReactive(input$calculate_budget,{
    pv   <- prof()
    cats <- if(nrow(user_cats())>0) user_cats() else pv$categories
    if(is.null(cats)||nrow(cats)==0) return(NULL)
    income_usd   <- as.numeric(pv$income[1])
    total_spent  <- sum(as.numeric(cats$amount))
    total_saved  <- max(income_usd-total_spent,1)
    savings_rate <- round((total_saved/income_usd)*100,1)
    cur          <- as.character(pv$currency[1])
    summary_df <- cats %>%
      mutate(amount=as.numeric(amount),
        pct_of_income=round((amount/income_usd)*100,1),
        status=case_when(
          amount/income_usd>=0.4 ~ "High spend",
          amount/income_usd>=0.2 ~ "Moderate",
          TRUE                   ~ "Low spend"))
    list(by_category=summary_df,total_income=income_usd,
         total_spent=total_spent,total_saved=total_saved,
         savings_rate=savings_rate,currency=cur)
  })
  output$budget_table <- DT::renderDataTable({
    req(budget_result())
    budget_result()$by_category %>%
      mutate(amount=dollar(amount),pct_of_income=paste0(pct_of_income,"%")) %>%
      rename(Category=category,`Monthly Amount`=amount,
             `% of Income`=pct_of_income,Status=status) %>%
      DT::datatable(options=list(dom="t",pageLength=20),rownames=FALSE) %>%
      DT::formatStyle("Status",backgroundColor=DT::styleEqual(
        c("High spend","Moderate","Low spend"),
        c("#ffcccc","#fff3cc","#ccffcc")))
  })
  output$total_income_box <- renderValueBox({
    req(budget_result())
    valueBox(paste0(budget_result()$currency," ",round(budget_result()$total_income,2)),
      "Monthly Income",icon=icon("money-bill"),color="blue")
  })
  output$total_spent_box <- renderValueBox({
    req(budget_result())
    valueBox(paste0(budget_result()$currency," ",round(budget_result()$total_spent,2)),
      "Total Budgeted",icon=icon("credit-card"),color="red")
  })
  output$total_saved_box <- renderValueBox({
    req(budget_result())
    valueBox(paste0(budget_result()$currency," ",round(budget_result()$total_saved,2)),
      "Available to Save",icon=icon("piggy-bank"),color="green")
  })
  output$savings_rate_box <- renderValueBox({
    req(budget_result())
    valueBox(paste0(budget_result()$savings_rate,"%"),
      "Savings Rate",icon=icon("chart-pie"),color="yellow")
  })
  output$spending_donut <- renderPlotly({
    req(budget_result())
    df <- budget_result()$by_category
    plot_ly(df,labels=~category,values=~amount,type="pie",hole=0.4,
      textinfo="label+percent") %>%
      layout(title="Where your money goes",showlegend=TRUE)
  })
  output$budget_vs_actual <- renderPlotly({
    req(budget_result())
    df <- budget_result()$by_category
    plot_ly(df,x=~category,y=~amount,type="bar",
      marker=list(color="#2196F3"),name="Your Budget") %>%
      layout(xaxis=list(title=""),yaxis=list(title="Amount"))
  })
  outlook_result <- eventReactive(input$calc_outlook,{
    six_month_projection_app(
      income_usd=input$outlook_income,monthly_spend_usd=input$outlook_spend,
      current_savings_usd=input$current_savings,extra_monthly_saving=input$extra_saving)
  })
  output$projection_chart <- renderPlotly({
    req(outlook_result())
    df <- outlook_result()
    plot_ly(df,x=~month_label,y=~cumulative,type="scatter",
      mode="lines+markers",line=list(color="#2196F3",width=3),
      marker=list(size=8)) %>%
      layout(xaxis=list(title="Month"),yaxis=list(title="Cumulative Savings"))
  })
  output$projection_table <- DT::renderDataTable({
    req(outlook_result())
    outlook_result() %>%
      mutate(net_this_month=dollar(net_this_month),cumulative=dollar(cumulative)) %>%
      rename(Month=month_label,Net=net_this_month,Savings=cumulative,Status=status) %>%
      select(-month) %>%
      DT::datatable(options=list(dom="t"),rownames=FALSE) %>%
      DT::formatStyle("Status",backgroundColor=DT::styleEqual(
        c("Saving","Deficit"),c("#ccffcc","#ffcccc")))
  })
  goal_result <- eventReactive(input$calc_goal,{
    req(input$goal_name,input$goal_amount,input$goal_months)
    cats <- if(nrow(user_cats())>0) user_cats() else prof()$categories
    if(is.null(cats)||nrow(cats)==0) return(NULL)
    goal_advisor_app(goal_name=input$goal_name,goal_amount=input$goal_amount,
      months_to_goal=input$goal_months,
      current_spending_df=cats %>% rename(amount_usd=amount),
      num_people=input$goal_people)
  })
  output$goal_message <- renderUI({
    req(goal_result())
    color <- if(goal_result()$achievable) "#ccffcc" else "#fff3cc"
    div(style=paste0("background:",color,";padding:12px;border-radius:6px;margin-bottom:12px;font-size:15px;"),
      goal_result()$message,tags$br(),tags$br(),
      tags$b("You need: "),paste0("$",goal_result()$monthly_needed,"/month"),
      if(input$goal_people>1) paste0(" ($",goal_result()$per_person_needed,"/person/month)"))
  })
  output$goal_cuts_table <- DT::renderDataTable({
    req(goal_result())
    cats <- if(nrow(user_cats())>0) user_cats() else prof()$categories
    if(is.null(cats)||nrow(cats)==0) return(NULL)
    cats %>%
      mutate(amount=as.numeric(amount),suggested_cut=round(amount*0.25,2),new_budget=round(amount*0.75,2)) %>%
      arrange(desc(suggested_cut)) %>%
      mutate(amount=dollar(amount),suggested_cut=dollar(suggested_cut),new_budget=dollar(new_budget)) %>%
      rename(Category=category,Current=amount,`Cut 25%`=suggested_cut,`New Budget`=new_budget) %>%
      DT::datatable(options=list(dom="t"),rownames=FALSE)
  })
  output$goals_ranked_table <- DT::renderDataTable({
    gd <- if(!is.null(auth$token)){
      tryCatch({
        resp <- request(paste0("https://xulmdwvkxqhtzaqrtzkr.supabase.co/rest/v1/goals?user_id=eq.",auth$user_id)) %>%
          req_headers("apikey"="eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6Inh1bG1kd3ZreHFodHphcXJ0emtyIiwicm9sZSI6ImFub24iLCJpYXQiOjE3NzUzNTI0OTgsImV4cCI6MjA5MDkyODQ5OH0.Quvtm1bj1S_DCbC6dmCpMUCBCaJu9clvEvke8LiEtDE",
                      "Authorization"=paste("Bearer",auth$token)) %>%
          req_perform()
        data <- resp %>% resp_body_json()
        if(length(data)>0) tibble(
          goal_name=sapply(data,function(x) x$goal_name),
          target_amount=sapply(data,function(x) as.numeric(x$target_amount)),
          saved_so_far=sapply(data,function(x) as.numeric(x$saved_so_far)),
          deadline=as.Date(sapply(data,function(x) x$deadline)),
          currency=sapply(data,function(x) x$currency),
          priority=1)
        else tibble(goal_name=character(),target_amount=numeric(),saved_so_far=numeric(),deadline=as.Date(character()),currency=character(),priority=numeric())
      },error=function(e) tibble(goal_name=character(),target_amount=numeric(),saved_so_far=numeric(),deadline=as.Date(character()),currency=character(),priority=numeric()))
    } else tibble(goal_name=character(),target_amount=numeric(),saved_so_far=numeric(),deadline=as.Date(character()),currency=character(),priority=numeric())
    if(nrow(gd)==0) return(DT::datatable(tibble(Message="No goals added yet."),options=list(dom="t"),rownames=FALSE))
    rank_goals_app(gd) %>%
      mutate(target_amount=dollar(target_amount),saved_so_far=dollar(saved_so_far),monthly_needed=dollar(monthly_needed)) %>%
      rename(Goal=goal_name,Target=target_amount,Saved=saved_so_far,Deadline=deadline,`Days Left`=days_left,Need=amount_needed,`Per Month`=monthly_needed,Urgency=urgency) %>%
      select(-currency,-priority) %>%
      DT::datatable(options=list(dom="t"),rownames=FALSE) %>%
      DT::formatStyle("Urgency",backgroundColor=DT::styleEqual(
        c("🔴 Urgent","🟡 Soon","🟢 On track"),
        c("#ffcccc","#fff3cc","#ccffcc")))
  })
  remit_result <- eventReactive(input$calc_remit,{
    from_amt <- as.numeric(input$remit_amount)
    from_cur <- input$remit_from
    to_cur   <- input$remit_to
    rates <- list(USD=1.00,GBP=1.27,INR=0.012,CNY=0.14)
    usd_val <- round(from_amt*rates[[from_cur]],2)
    to_amt  <- round(usd_val/rates[[to_cur]],2)
    list(from_amount=from_amt,from_curr=from_cur,to_amount=to_amt,to_curr=to_cur,usd_value=usd_val)
  })
  output$remit_result <- renderUI({
    req(remit_result())
    rv <- remit_result()
    div(style="font-size:16px;padding:20px;",
      div(style="background:#e3f2fd;padding:16px;border-radius:8px;margin-bottom:12px;",
        tags$h3(paste0(rv$from_amount," ",rv$from_curr,"  ->  ",rv$to_amount," ",rv$to_curr)),
        tags$p(paste0("USD equivalent: $",rv$usd_value))),
      tags$p(style="color:#666;font-size:13px;","Tip: Compare rates on Wise or Remitly before sending."))
  })
  split_result <- eventReactive(input$calc_split,{
    req(input$expense_desc,input$expense_amount,input$expense_members,input$expense_paid_by)
    members <- trimws(strsplit(input$expense_members,",")[[1]])
    split_expense_app(description=input$expense_desc,total_amount=as.numeric(input$expense_amount),
      currency=input$expense_currency,paid_by=input$expense_paid_by,members=members,split_type="equal")
  })
  output$split_result <- renderUI({
    req(split_result())
    sv <- split_result()
    div(style="background:#f3e5f5;padding:16px;border-radius:8px;margin-bottom:12px;",
      tags$h4(paste0("Split: ",sv$description)),
      tags$p(paste0("Total: ",sv$from_amount," ",sv$from_curr," = $",sv$total_usd," USD")),
      tags$p(paste0("Paid by: ",sv$paid_by)))
  })
  output$split_table <- DT::renderDataTable({
    req(split_result())
    split_result()$splits %>%
      mutate(owes_usd=dollar(owes_usd)) %>%
      rename(Member=member,`Owes (USD)`=owes_usd,`Owes (local)`=owes_local,Status=status) %>%
      DT::datatable(options=list(dom="t"),rownames=FALSE) %>%
      DT::formatStyle("Status",backgroundColor=DT::styleEqual(
        c("Paid ✅","Owes 💸"),c("#ccffcc","#ffcccc")))
  })


  # Group creation and joining
  observeEvent(input$btn_create_group,{
    req(auth$token, auth$user_id)
    group_name <- if(!is.null(input$setup_name) && input$setup_name!="") input$setup_name else "My Group"
    result <- supabase_create_group_app(auth$token, auth$user_id, paste0(group_name,"s Group"))
    if(result$success){
      output$group_status <- renderUI(
        div(style="background:#ccffcc;padding:10px;border-radius:6px;margin-top:10px;",
          tags$p(tags$b("Group created!")),
          tags$p("Share this invite code with your roommates/partner:"),
          tags$h3(style="color:#0066cc;letter-spacing:3px;",result$invite_code),
          tags$p(style="color:#666;font-size:12px;","They can enter this code in their Setup Profile to join your group.")))
    } else {
      output$group_status <- renderUI(
        div(style="background:#ffcccc;padding:10px;border-radius:6px;margin-top:10px;",
          "Failed to create group. Please try again."))
    }
  })

  observeEvent(input$btn_join_group,{
    req(auth$token, auth$user_id, input$join_code)
    result <- supabase_join_group_app(auth$token, auth$user_id, trimws(input$join_code))
    if(result$success){
      output$group_status <- renderUI(
        div(style="background:#ccffcc;padding:10px;border-radius:6px;margin-top:10px;",
          paste0("Successfully joined group: ", result$group_name, "!")))
    } else {
      output$group_status <- renderUI(
        div(style="background:#ffcccc;padding:10px;border-radius:6px;margin-top:10px;",
          paste0("Error: ", result$message)))
    }
  })
}
