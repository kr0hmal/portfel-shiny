# =============================================================================
# PORTFEL - Analizator Portfela Inwestycyjnego (szkielet, wersja 1)
# Zakres tej wersji:
#   F-01 definiowanie portfela, F-02 okres analizy,
#   F-03 pobieranie notowań,    F-04 wykres wartości portfela
# =============================================================================

library(shiny)
library(bslib)
library(quantmod)
library(plotly)

# ---- 1. FUNKCJE POMOCNICZE (logika trzymamy osobno od interfejsu) -----------

# Zamienia tekst "AAPL, msft" na wektor c("AAPL", "MSFT")
parsuj_tickery <- function(tekst) {
  tickery <- toupper(trimws(strsplit(tekst, ",")[[1]]))
  tickery[tickery != ""]
}

# Zamienia tekst "40, 30, 20, 10" na wektor liczb (błędny zapis daje NA)
parsuj_wagi <- function(tekst) {
  suppressWarnings(as.numeric(trimws(strsplit(tekst, ",")[[1]])))
}

# Pobiera skorygowane ceny zamknięcia z Yahoo Finance dla jednego tickera
pobierz_ceny <- function(ticker, data_od, data_do) {
  dane <- getSymbols(ticker, src = "yahoo", from = data_od, to = data_do,
                     auto.assign = FALSE)
  Ad(dane)
}

# Pobiera ceny dla wszystkich tickerów i łączy je w jedną tabelę
pobierz_ceny_portfela <- function(tickery, data_od, data_do) {
  lista <- lapply(tickery, function(ticker) {
    ceny <- tryCatch(
      pobierz_ceny(ticker, data_od, data_do),
      error = function(e) NULL
    )
    if (is.null(ceny) || nrow(ceny) < 2) {
      stop(paste0("Nie udało się pobrać danych dla tickera: ", ticker))
    }
    colnames(ceny) <- ticker
    ceny
  })
  na.omit(do.call(merge, lista))
}

# Liczy wartość portfela w czasie (założenie: stałe wagi, rebalansowanie dzienne)
policz_wartosc_portfela <- function(ceny, wagi, kwota) {
  # dzienne stopy zwrotu każdego aktywa (pierwszy wiersz to NA, więc go usuwamy)
  zwroty <- na.omit(ROC(ceny, type = "discrete"))
  
  # dzienny zwrot portfela = suma (waga * zwrot aktywa)
  zwrot_portfela <- as.numeric(coredata(zwroty) %*% wagi)
  
  # wartość portfela = kwota początkowa * skumulowane zwroty
  data.frame(
    data = index(zwroty),
    wartosc = kwota * cumprod(1 + zwrot_portfela)
  )
}

# ---- 2. INTERFEJS (ui) ------------------------------------------------------

ui <- page_sidebar(
  title = "PORTFEL - Analizator Portfela Inwestycyjnego",
  theme = bs_theme(bootswatch = "flatly"),
  
  sidebar = sidebar(
    width = 300,
    textInput("tickery", "Tickery (oddzielone przecinkami)",
              value = "AAPL, MSFT, KO, GLD"),
    textInput("wagi", "Wagi w % (w tej samej kolejności)",
              value = "40, 30, 20, 10"),
    numericInput("kwota", "Kwota początkowa",
                 value = 10000, min = 1, step = 1000),
    dateRangeInput("okres", "Okres analizy",
                   start = Sys.Date() - 3 * 365, end = Sys.Date(),
                   max = Sys.Date()),
    actionButton("analizuj", "Analizuj", class = "btn-primary")
  ),
  
  card(
    card_header("Wartość portfela w czasie"),
    plotlyOutput("wykres_wartosci")
  )
)

# ---- 3. LOGIKA (server) -----------------------------------------------------

server <- function(input, output, session) {
  
  # eventReactive: obliczenia ruszają po kliknięciu "Analizuj"
  # (ignoreNULL = FALSE sprawia, że wykres pokazuje się też od razu po starcie)
  wartosc_portfela <- eventReactive(input$analizuj, {
    tickery <- parsuj_tickery(input$tickery)
    wagi <- parsuj_wagi(input$wagi)
    
    # walidacja danych od użytkownika - komunikat pojawia się zamiast wykresu
    validate(
      need(length(tickery) > 0,
           "Wpisz co najmniej jeden ticker."),
      need(!anyDuplicated(tickery),
           "Tickery nie mogą się powtarzać."),
      need(length(tickery) == length(wagi) && !anyNA(wagi),
           "Liczba wag musi być równa liczbie tickerów, a wagi muszą być liczbami."),
      need(all(wagi >= 0),
           "Wagi nie mogą być ujemne."),
      need(abs(sum(wagi) - 100) < 0.01,
           paste0("Suma wag musi wynosić 100% (teraz: ", sum(wagi), "%).")),
      need(!is.na(input$kwota) && input$kwota > 0,
           "Kwota początkowa musi być dodatnia.")
    )
    
    # pobieranie danych z paskiem postępu i obsługą błędów
    ceny <- withProgress(message = "Pobieram notowania...", value = 0.5, {
      tryCatch(
        pobierz_ceny_portfela(tickery, input$okres[1], input$okres[2]),
        error = function(e) validate(need(FALSE, conditionMessage(e)))
      )
    })
    
    policz_wartosc_portfela(ceny, wagi / 100, input$kwota)
  }, ignoreNULL = FALSE)
  
  output$wykres_wartosci <- renderPlotly({
    df <- wartosc_portfela()
    
    plot_ly(df, x = ~data, y = ~wartosc, type = "scatter", mode = "lines",
            name = "Portfel", line = list(color = "#2a78d6", width = 2)) %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Wartość portfela", tickformat = ",.0f"),
        hovermode = "x unified"
      )
  })
}

# ---- 4. URUCHOMIENIE --------------------------------------------------------

shinyApp(ui, server)
