# ============================================================
# FILE: ui_app.R
# PURPOSE: User interface for Budget Tracker App
# ============================================================

ui <- dashboardPage(
  dashboardHeader(title = "Budget Tracker App"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
      menuItem("Login / Sign Up", tabName="login", icon=icon("right-to-bracket")),
      menuItem("Setup Profile",  tabName="setup",      icon=icon("user")),
      menuItem("My Budget",      tabName="budget",     icon=icon("wallet")),
      menuItem("6-Month Outlook",tabName="outlook",    icon=icon("chart-line")),
      menuItem("Goal Planner",   tabName="goals",      icon=icon("bullseye")),
      menuItem("Remittance",     tabName="remittance", icon=icon("money-bill-transfer")),
      menuItem("Group Expenses", tabName="group",      icon=icon("people-group"))
    ),
    hr(),
    radioButtons("app_mode", "Who is this for?",
      choices  = c("Just me"="individual","Me + partner/roommates"="group"),
      selected = "individual")
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName="login",
        fluidRow(
          box(title="Welcome to Budget Tracker App",width=6,status="primary",solidHeader=TRUE,
            offset=3,
            tags$h4("Sign up or log in to get started"),
            tags$hr(),
            textInput("auth_email","Email address",placeholder="your@email.com"),
            passwordInput("auth_password","Password",placeholder="minimum 6 characters"),
            fluidRow(
              column(6,actionButton("btn_login","Log In",class="btn-primary btn-block")),
              column(6,actionButton("btn_signup","Sign Up",class="btn-success btn-block"))
            ),
            br(),
            uiOutput("auth_message")
          )
        )
      ),

      # ── SETUP PAGE ──────────────────────────────────────
      tabItem(tabName="setup",
        fluidRow(
          box(title="Step 1 — About You",width=6,status="primary",solidHeader=TRUE,
            textInput("setup_name","Your name",""),
            numericInput("setup_income","Your monthly income",3000,min=0,step=100),
            selectInput("setup_currency","Your currency",
              c("USD","GBP","INR","CNY"),"USD"),
            radioButtons("setup_mode","Who is this for?",
              c("Just me"="individual","Me + partner/roommates"="group"),
              "individual")
            ,conditionalPanel(
              condition = "input.setup_mode == 'group'",
              hr(),
              tags$p(tags$b("Group Setup"), style="margin-top:10px;"),
              actionButton("btn_create_group","Create New Group",class="btn-info btn-block"),
              br(),br(),
              textInput("join_code","Or enter invite code to join existing group",
                placeholder="e.g. ABCD1234"),
              actionButton("btn_join_group","Join Group",class="btn-warning btn-block"),
              br(),
              uiOutput("group_status")
            )
          ),
          box(title="Step 2 — Set Minimum Savings",width=6,status="success",solidHeader=TRUE,
            p("The app will always protect this amount every month."),
            numericInput("setup_min_savings",
              "Minimum I want to save each month ($)",50,min=1,step=10),
            p(style="color:#888;font-size:13px;",
              "Even $1 counts. The app will warn you if you go below this.")
          )
        ),
        fluidRow(
          box(title="Step 3 — Your Monthly Expenses",width=12,status="warning",solidHeader=TRUE,
            p("Add every category you spend on each month. You can add as many as you need."),
            fluidRow(
              column(4, textInput("new_cat_name","Category name",
                placeholder="e.g. Fuel, Gym, Baby food")),
              column(4, numericInput("new_cat_amount","Monthly amount ($)",
                100,min=0,step=10)),
              column(4, br(),
                actionButton("add_category","+ Add Category",
                  class="btn-warning btn-block"))
            ),
            hr(),
            DTOutput("categories_preview"),
            hr(),
            actionButton("save_profile","💾 Save My Profile",
              class="btn-success btn-block"),
            br(),
            uiOutput("setup_message")
          )
        )
      ),

      # ── MY BUDGET ───────────────────────────────────────
      tabItem(tabName="budget",
        uiOutput("profile_banner"),
        fluidRow(
          box(title="Your Monthly Income",width=4,status="primary",solidHeader=TRUE,
            uiOutput("budget_income_display"),
            actionButton("calculate_budget","Calculate My Budget",
              class="btn-primary btn-block")
          ),
          box(title="Budget Summary",width=8,status="success",solidHeader=TRUE,
            DTOutput("budget_table"))
        ),
        fluidRow(
          valueBoxOutput("total_income_box", width=3),
          valueBoxOutput("total_spent_box",  width=3),
          valueBoxOutput("total_saved_box",  width=3),
          valueBoxOutput("savings_rate_box", width=3)
        ),
        fluidRow(
          box(title="Spending by Category",width=6,status="info",solidHeader=TRUE,
            plotlyOutput("spending_donut")),
          box(title="Budget vs Actual",width=6,status="warning",solidHeader=TRUE,
            plotlyOutput("budget_vs_actual"))
        )
      ),

      # ── 6-MONTH OUTLOOK ─────────────────────────────────
      tabItem(tabName="outlook",
        fluidRow(
          box(title="Your Numbers",width=4,status="primary",solidHeader=TRUE,
            numericInput("outlook_income","Monthly income (USD)",3000,min=0,step=100),
            numericInput("outlook_spend","Avg monthly spending (USD)",2500,min=0,step=100),
            numericInput("current_savings","Current savings (USD)",500,min=0,step=50),
            numericInput("extra_saving","Extra saving per month (USD)",0,min=0,step=10),
            actionButton("calc_outlook","Show My Outlook",class="btn-primary btn-block")
          ),
          box(title="6-Month Savings Projection",width=8,status="success",solidHeader=TRUE,
            plotlyOutput("projection_chart"),
            DTOutput("projection_table"))
        )
      ),

      # ── GOAL PLANNER ────────────────────────────────────
      tabItem(tabName="goals",
        fluidRow(
          box(title="Add a New Goal",width=4,status="primary",solidHeader=TRUE,
            textInput("goal_name","What are you saving for?",
              placeholder="e.g. Engagement ring, new shoes, trip..."),
            numericInput("goal_amount","How much do you need? (USD)",1000,min=1,step=50),
            numericInput("goal_months","How many months do you have?",6,min=1,max=60,step=1),
            numericInput("goal_people","How many people saving together?",1,min=1,max=5,step=1),
            actionButton("calc_goal","Show Me How",class="btn-success btn-block")
          ),
          box(title="Goal Advice",width=8,status="success",solidHeader=TRUE,
            uiOutput("goal_message"),
            DTOutput("goal_cuts_table"))
        ),
        fluidRow(
          box(title="Your Goal Priority List",width=12,status="warning",solidHeader=TRUE,
            DTOutput("goals_ranked_table"))
        )
      ),

      # ── REMITTANCE ──────────────────────────────────────
      tabItem(tabName="remittance",
        fluidRow(
          box(title="Send Money Home",width=4,status="primary",solidHeader=TRUE,
            numericInput("remit_amount","Amount you want to send",200,min=1,step=10),
            selectInput("remit_from","You are sending from",c("USD","GBP","CNY"),"USD"),
            selectInput("remit_to","Family receives in",c("INR","CNY","GBP","USD"),"INR"),
            actionButton("calc_remit","Convert",class="btn-primary btn-block")
          ),
          box(title="Conversion Result",width=8,status="success",solidHeader=TRUE,
            uiOutput("remit_result"))
        )
      ),

      # ── GROUP EXPENSES ──────────────────────────────────
      tabItem(tabName="group",
        fluidRow(
          box(title="Split an Expense",width=4,status="primary",solidHeader=TRUE,
            textInput("expense_desc","What was this expense?",
              placeholder="e.g. Dinner, Rent, Groceries"),
            numericInput("expense_amount","Total amount",100,min=1,step=5),
            selectInput("expense_currency","Currency",c("USD","GBP","INR","CNY"),"USD"),
            textInput("expense_members","Member names (comma separated)",
              placeholder="Alice, Bob, Carol"),
            textInput("expense_paid_by","Who paid?",placeholder="Alice"),
            actionButton("calc_split","Split It",class="btn-success btn-block")
          ),
          box(title="Split Result",width=8,status="success",solidHeader=TRUE,
            uiOutput("split_result"),
            DTOutput("split_table"))
        )
      )
    )
  )
)




con <- file("app/server_app.R", "a")
writeLines('', con)
writeLines('  # Group creation and joining', con)
writeLines('  observeEvent(input$btn_create_group,{', con)
writeLines('    req(auth$token, auth$user_id)', con)
writeLines('    group_name <- if(!is.null(input$setup_name) && input$setup_name!="") input$setup_name else "My Group"', con)
writeLines('    result <- supabase_create_group_app(auth$token, auth$user_id, paste0(group_name,"s Group"))', con)
writeLines('    if(result$success){', con)
writeLines('      output$group_status <- renderUI(', con)
writeLines('        div(style="background:#ccffcc;padding:10px;border-radius:6px;margin-top:10px;",', con)
writeLines('          tags$p(tags$b("Group created!")),', con)
writeLines('          tags$p("Share this invite code with your roommates/partner:"),', con)
writeLines('          tags$h3(style="color:#0066cc;letter-spacing:3px;",result$invite_code),', con)
writeLines('          tags$p(style="color:#666;font-size:12px;","They can enter this code in their Setup Profile to join your group.")))', con)
writeLines('    } else {', con)
writeLines('      output$group_status <- renderUI(', con)
writeLines('        div(style="background:#ffcccc;padding:10px;border-radius:6px;margin-top:10px;",', con)
writeLines('          "Failed to create group. Please try again."))', con)
writeLines('    }', con)
writeLines('  })', con)
writeLines('', con)
writeLines('  observeEvent(input$btn_join_group,{', con)
writeLines('    req(auth$token, auth$user_id, input$join_code)', con)
writeLines('    result <- supabase_join_group_app(auth$token, auth$user_id, trimws(input$join_code))', con)
writeLines('    if(result$success){', con)
writeLines('      output$group_status <- renderUI(', con)
writeLines('        div(style="background:#ccffcc;padding:10px;border-radius:6px;margin-top:10px;",', con)
writeLines('          paste0("Successfully joined group: ", result$group_name, "!")))', con)
writeLines('    } else {', con)
writeLines('      output$group_status <- renderUI(', con)
writeLines('        div(style="background:#ffcccc;padding:10px;border-radius:6px;margin-top:10px;",', con)
writeLines('          paste0("Error: ", result$message)))', con)
writeLines('    }', con)
writeLines('  })', con)
writeLines('}', con)
close(con)
message("✅ Group server logic added! Lines: ", readLines("app/server_app.R") |> length())



readLines("app/server_app.R")[294:300]
lines <- readLines("app/server_app.R")
lines[297] <- ''
writeLines(lines, "app/server_app.R")
message("✅ Fixed!")
source("R/functions_app.R")
source("R/supabase_app.R")
server <- source("app/server_app.R", local=TRUE)$value
message("server OK")

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(DT)
ui     <- source("app/ui_app.R", local=TRUE)$value
shinyApp(ui=ui, server=server)



rsconnect::deployApp(
  appDir  = ".",
  appFiles = c(
    "app.R",
    "app/ui_app.R",
    "app/server_app.R",
    "R/functions_app.R",
    "R/supabase_app.R",
    "data/transactions_app.csv",
    "data/budgets_app.csv",
    "data/goals_app.csv",
    "data/group_members_app.csv",
    "data/group_expenses_app.csv",
    "data/profile_app.csv",
    "data/user_categories_app.csv"
  ),
  appName = "budget-tracker-app",
  account = "budgettrackerappnl"
)