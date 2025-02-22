# 1. Stwórz funkcję o nazwie kostka, która będzie symulować n rzutów kostką.
# Wskazówka: Użyj funkcji sample() do losowania liczby oczek od 1 do 6.

kostka <- function(n) {
  sample(1:6, n, replace = TRUE)
}

kostka(3)

# 2. Stwórz funkcję, która będzie tworzyć wektor o zadanej długości.
# Funkcja ma zwracać wektor liczb całkowitych od 1 do n:
#  długość wektora wynosi n, a wartości w wektorze to sekwencja liczb od 1 do n.

wektor <- function(n) {
  return(1:n)
}

wektor(3)

# 3. Stwórz funkcję o nazwie pole_kola, która oblicza pole powierzchni koła dla danego promienia.

pole_kola <- function(r) {
  return(pi*r^2)
}

pole_kola(3)

# 4. Stwórz funkcję, która oblicza długość przeciwprostokątnej w trójkącie prostokątnym.

c <- function(a,b) {
  return(sqrt(a^2 + b^2))
}

c(3,4)

# 5. Stwórz funkcję będącą najprostszą wersją kalkulatora 
# (dodawanie, odejmowanie, mnożenie albo dzielenie dwóch liczb).

calc <- function(operator,a,b) {
  switch(operator, 
         "+" = return(a+b),
         "-" = return(a-b),
         "*" = return(a*b),
         "/" = if(b!=0) return(a/b)
         else "Error: Division by zero...",
         "Error: Unknown operator..."
  )
}

calc("+",55,89)

# 6. Stwórz funkcję o nazwie przyznaj_nagrode()
# która symuluje rzut sześcienną kostką do gry i przyznaje nagrodę w zależności od wyniku rzutu. 
# Funkcja powinna działać według następujących zasad:
# - Jeśli wyrzucona liczba oczek to 6, funkcja powinna zwrócić komunikat: "Super bonus!"
# - Jeśli wyrzucona liczba oczek to 4 lub 5, funkcja powinna zwrócić komunikat: "Nagroda standardowa"
# - Jeśli wyrzucona liczba oczek to 1, 2 lub 3, funkcja powinna zwrócić komunikat: "Brak nagrody..."

przyznaj_nagrode <- function() {
  wynik <- sample(1:6, 1)
  if(wynik == 6) "Super bonus!"
  else if(wynik == 4 || wynik == 5) "Nagroda standardowa"
  else "Brak nagrody..."
}

przyznaj_nagrode()

# 7. Stwórz funkcję obliczającą podatek w zależności od dochodu. 
# Przyjmij następujące założenia:
# a) Jeżeli podatnik rozlicza się liniowo, wtedy niezależnie od kwoty płaci 19% podatku.
# b) Jeżeli podatnik rozlicza się na zasadach ogólnych, wtedy:
# - poniżej kwoty 85528zł płaci 18% podatku minus kwota zmniejszająca, czyli 556zł;
# - powyżej kwoty 85528zł płaci 14839zł + 32% nadwyżki powyżej 85528zł.

tax <- function(system,income) {
  if(income < 0) return("Error: Income < 0")
  
  if(system == "liniowy") return(0.19*income)
  else if(system == "ogolny") {
    if(income < 85528) return(0.18*income - 556)
    else return(14839 + 0.32*(income-85528))
  }
  else return("Error: Unknown system")
}

tax("ogolny",100000)