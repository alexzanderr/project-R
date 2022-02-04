

# Instructions
tot ce aveti nevoie este in folderul `raport`. pace!

acolo gasiti:
- `raport.Rmd`
- `raport.html`
- `raport.docx`

atat aveti nevoie

# cum clonati repo la voi pe PC
```shell
git clone https://github.com/alexzanderr/project-R
cd project-R
```

# Bibliografie
pentru R:
- https://www.statology.org/remove-element-from-vector-r/
- https://stackoverflow.com/questions/13841599/calculate-frequency-of-occurrence-in-an-array-using-r
- https://r-coder.com/plot-r/#R_plot_color
- https://www.tutorialspoint.com/how-to-change-the-background-color-of-a-plot-created-by-using-plot-function-in-r
- https://r-charts.com/base-r/background-color/
- https://plotly.com/r/shapes/
- https://www.rdocumentation.org/packages/LaplacesDemon/versions/16.1.6/topics/RejectionSampling
- https://www.youtube.com/watch?v=6r-2N_bJdck
- https://stackoverflow.com/questions/8708243/import-stuff-from-a-r-file
- https://github.com/REditorSupport/languageserver#installation
- https://github.com/sublimelsp/LSP-R
- https://r-lang.com/r-append-to-list-how-to-append-element-in-r-list/
- https://www.w3schools.com/r/r_for_loop.asp
- https://stackoverflow.com/a/66996483/12172291
- https://www.tutorialspoint.com/how-to-create-a-random-sample-of-values-between-0-and-1-in-r
- https://www.w3schools.com/r/r_graph_plot.asp
- https://stackoverflow.com/questions/20454789/how-do-i-tell-r-to-fill-the-circle-dots-with-colour-on-a-scatter-plot
- https://www.statology.org/r-count-number-of-occurrences-in-column/
- https://www.statology.org/random-sample-in-r/
- https://rmarkdown.rstudio.com/lesson-9.html
- https://www.tutorialspoint.com/r/r_data_frames.htm
- https://www.youtube.com/watch?v=kMb4JlvuGlw
- https://stackoverflow.com/questions/12162278/appending-column-to-a-data-frame-r
- https://www.tutorialspoint.com/r/r_strings.htm



pentru statistica:
- https://www.youtube.com/watch?v=Dzqe1a_AIkc
- https://www.vedantu.com/formula/probability-formula

```R
isPowerOf2 <- function(x) {
  n1s <- sum(as.numeric(intToBits(x)))
  if (n1s == 1) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
```

# results

problema 1 - proc 1 `FARA` swap la x and y -> 0.13 probability always
<img src=problema1-fara-swap.png>

problema 1 - proc 1 `CU` swap la x and y -> 0.26 probability always
<img src=problema1-cu-swap.png>

problema 1 - proc 2 `FARA` swap la x and y -> 0.21 probability always
<img src="prob1-procedura2-fara-swap.png">

problema 1 - proc 2 `CU` swap la x and y ~= 0.40 probability always
<img src="prob1-procedura2-cu-swap.png">


# ce a mai ramas de facut la problema 1
[1]

rezultatul la punctul anterior adica prob ~= 0.26% intotdeauna

[3]
procedura 1 - 4 - justificati teoretic probabilitatea de approx 0.68% triunghiuri obtuze mereu


[4]
procedura 2 - 5 - subpunctul c de mai sus, probabilitatea teoretic

[5]
procedura 2 - 6 - faza cu Z


[6]
procedura 3 - 5 - punctul c iara cu probabilitatea teoretic
procedura 3 - 6 - faza cu Z


1. ca sa pot sa fac o observatie pentru f inseamna ca f caciula trebuie sa fie eligibil pentru rejection sampling

## prerequisites
- Rmarkdown package
- https://rmarkdown.rstudio.com/lesson-1.html
- https://rmarkdown.rstudio.com/authoring_quick_tour.html
- https://bookdown.org/yihui/rmarkdown/
- toate simularile, codul si ploturile sa fie incluse in raport
- raportul poate fi HTML, word, sau LateX si pe langa raport trebuie sa ai si .Rmd
