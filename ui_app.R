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
