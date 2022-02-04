

get_min_max_2nd_min <- function(a, b, c) {
    # punem valorile intr-un array
    values <- c(a, b, c)
    # luam minim
    minimum <- min(values)
    # luam maxim
    maximum <- max(values)
    # alfam al doilea min
    # '! values %in% c(minimum)' inseamna toate valorile care nu sunt in vectorul c(minimum)
    # adica doar vectorul cu valoarea minima
    second_min <- min(values[! values %in% c(minimum)])
    # returnam un vector to toate valorile in ordine crescatoare
    # bune pentru formula de triunghi obtuz
    return (c(minimum, second_min, maximum))
}

is_obtuse_triangle <- function(a, b, c) {
    # formula este
    # min^2 + second_min^2 < max^2 -> if TRUE then obtuz else 'nu e obtuz'

    # luam valorile si le ridicam la patrat pentru formula
    values <- get_min_max_2nd_min(a, b, c) ^ 2
    # membrul stang al egalitatii
    min_sum <- values[1] + values[2]

    # daca suma minimelor < max^2
    if (min_sum < values[3]) {
        # obtuz
        return (TRUE)
    }
    # non-obtuz
    return (FALSE)
}


is_triangle <- function(a, b, c) {
    # daca toate laturile sunt mai mici decat 1 pe 2 (0.5)
    if (a <= 0.5 && b <= 0.5 && c <= 0.5) {
        # triunghi
        return (TRUE)
    }
    # non-triunghi
    return (FALSE)
}



is_triangle_method_1 <- function(a, b, c) {
    if (a + b > c && a + c > b && b + c > a) {
        return (TRUE)
    }
    return (FALSE)
}

is_triangle_method_2 <- function(a, b, c) {
    if (a <= 0.5 && b <= 0.5 && c <= 0.5) {
        return (TRUE)
    }
    return (FALSE)
}


generate_xy <- function(total = 5000) {
    # 5000 individual unique random values between [0, 1]
    x <- runif(total, 0, 1)
    # 5000 individual unique random values between [0, 1]
    y <- runif(total, 0, 1)
    # why dataframe?
    # its much easier to manage
    return (data.frame(x = x, y = y))
}



generate_dataframe <- function(
    dataframe,
    swap = FALSE
) {
    # avem nevoie de un dataframe cu toate valorile respective pe coloane
    # pentru a le avea pe toate intr-o singura colectie sa fie mai usor de operat
    pairs = list(list(dataframe$x), list(dataframe$y))
    # culori, rosu sau albastru
    colors = c()
    # valori bool pentru triunghi
    triangle = c()
    # valori bool pentru triunghi obtuz
    obtuse = c()
    # array urile cu lungimi
    aa <- c()
    bb <- c()
    cc <- c()
    for (iter in seq_len(length(dataframe$y))) {
        # print(iter)
        x <- pairs[[1]][[1]][iter]
        y <- pairs[[2]][[1]][iter]

        # le face swap pentru nu are sens
        # dpdv matematic si alfabetic sa fie X > Y
        # cand pe axa avem in ordine (0, X) (X, Y) (Y, 1)
        if (swap) {
            if (x > y) {
                z <- x
                x <- y
                y <- z
            }
        }

        # generam lungimile pentru un triunghi
        a <- x
        b <- abs(x - y)
        c <- abs(1 - y)

        # punem valorile in array uri
        aa <- append(aa, a)
        bb <- append(bb, b)
        cc <- append(cc, c)

        # check if the a, b, c lengths form a triangle
        if (is_triangle(a, b, c)) {
            triangle <- append(triangle, TRUE)

            # check if the a, b, c lengths form an obtuse triangle
            if (is_obtuse_triangle(a, b, c)) {
                obtuse <- append(obtuse, TRUE)
                # am pus cele obtuze cu galben sa fie mai usor
                # de vizualizat
                colors <- append(colors, "#E5C07B")
            } else {
                colors <- append(colors, "#61AFEF")
                obtuse <- append(obtuse, FALSE)
            }
        } else {
            # have to initialize with default values
            colors <- append(colors, "#E06C75")
            triangle <- append(triangle, FALSE)
            obtuse <- append(obtuse, FALSE)
        }
    }
    # punem toate coloanele in dataframe
    dataframe[,"a"] <- aa
    dataframe[,"b"] <- bb
    dataframe[,"c"] <- cc
    dataframe[,"color"] <- colors
    dataframe[,"triangle"] <- triangle
    dataframe[,"obtuse"] <- obtuse
    return (dataframe)
}


# functia pentru probabilitatea de triunghiuri
get_total_triangles_probability <- function(dataframe, total) {
    # numaram in dataframe care valori sunt true in vectorul de boolene
    total_triangles = length(which(dataframe$triangle==TRUE))
    # cazuri fav pe posibile
    return (total_triangles / total)
}


# probabilitatea pentru obtuze din toate puncturile
get_total_obtuses_probability_from_all_points <- function(dataframe, total) {
    # numaram in dataframe care valori sunt true in vectorul de boolene
    total_triangles = length(which(dataframe$obtuse==TRUE))
    # cazuri fav pe posibile
    # probabilitatea pentru triunghiuri obtuze din toate toate punctele
    return (total_triangles / total)
}

# functia pentru probabilitatea de triunghiuri obtuze
get_total_obtuses_probability_from_all_triangles <- function(dataframe, total) {
    # probabilitatea pentru triunghiuri obtuze din toate triunghiurile
    total_triangles = length(which(dataframe$triangle==TRUE))
    total_obtuses = length(which(dataframe$obtuse==TRUE))
    return (total_obtuses / total_triangles)
}

# functia care face media lungimilor a, b, c
get_mean_lengths <- function(dataframe) {
    # punctul 2

    # mediile pentru (a1, a2, ..., an)
    as_mean = paste("all a's mean: ", mean(dataframe$a), sep="")
    print(as_mean)
    # mediile pentru (b1, b2, ..., bn)
    bs_mean = paste("all b's mean: ", mean(dataframe$b), sep="")
    print(bs_mean)
    # mediile pentru (c1, c2, ..., cn)
    cs_mean = paste("all c's mean: ", mean(dataframe$c), sep="")
    print(cs_mean)

    # individual means for the first 5

    for (i in 1:5) {
        a = dataframe$a[i]
        b = dataframe$b[i]
        c = dataframe$c[i]
        cat(a, b, c, "\n")
        # mediile pentru (a, b, c) pe rand
        mean_a_b_c = mean(c(a, b, c))
        print(mean_a_b_c)
    }
}




submain <- function(total, triangle_func, filename, swap) {
    # generam valorile x si y
    xy <- generate_xy(total)
    # print(head(xy))

    # cream dataframe cu tot ce am obtinut
    dataframe <- generate_dataframe(
        xy,
        # am plotat si fara swap pentru a vedea differenta
        swap=swap)
    # print(head(dataframe))

    total_triangles <- length(which(dataframe$triangle==TRUE))
    # calcuam probabilitatea pentru formare triunghiuri
    triangle_prob <- get_total_triangles_probability(dataframe, total)

    total_obtuses <- length(which(dataframe$obtuse==TRUE))
    # punctul 4
    # calcuam probabilitatea pentru formare triunghiuri obtuze din toate punctele
    obtuses_prob_from_all_points <- get_total_obtuses_probability_from_all_points(dataframe, total)
    # calcuam probabilitatea pentru formare triunghiuri obtuze doar din triunghiuri
    obtuses_prob_from_triangles <- get_total_obtuses_probability_from_all_triangles(dataframe, total)

    # adaugam titlu pentru plot
    main_text <- paste(
        "(X, Y) @ [0, 1] -> range: ",
        total,
        "\n",
        "total_triangles: ",
        total_triangles,
        " | ",
        triangle_prob, "%",
        "\n",
        "total_obtuses: ",
        total_obtuses,
        " | ",
        obtuses_prob_from_all_points, "%",
        " | ",
        obtuses_prob_from_triangles, "%",
        sep="")

    # punctul 3.a
    size <- 600
    white <- "#ABB2BF"
    # cream un png cu plotul
    png(file = filename,
        width = size,
        height = size)
    par(bg = "#282C34", fg=white)

    # plotam rezultatele finale
    plot(
        x = dataframe$x,
        y = dataframe$y,
        xlab = "X",
        ylab = "Y",
        xlim = c(0,1.1),
        ylim = c(0,1.1),
        main = main_text,
        col.main = white,
        col.sub = white,       # Subtitle color
        col.lab = white,    # X and Y-axis labels color
        col.axis = white,   # Tick labels color
        col = dataframe$color,
        pch = 16,
        cex = 1.5,
        cex.main = 1.5,   # Title size
        cex.sub = 0.5,  # Subtitle size
        cex.lab = 2,   # X-axis and Y-axis labels size
        cex.axis = 1, # Axis labels size
        # family = "monaco"
    )
    dev.off()

    # means for a, b, c
    get_mean_lengths(dataframe)


    # punctul 3.b
    # triangle_prob_string = paste("probability for triangles: ", triangle_prob, "%", sep="")
    # print(triangle_prob_string)
    # obtuse_prob_string = paste("probability for obtuses from all points: ", obtuses_prob_from_all_points, "%", sep="")
    # print(obtuse_prob_string)
    # obtuse_prob_string = paste("probability for obtuses from all triangles: ", obtuses_prob_from_triangles, "%", sep="")
    # print(obtuse_prob_string)
    # print(length(which(dataframe$x==0.33)))
}

raport_workpace <- function() {
    # generam valorile x si y
    xy <- generate_xy(total)

    # cream dataframe cu tot ce am obtinut
    dataframe <- generate_dataframe(
        xy,
        # am plotat si fara swap pentru a vedea differenta
        swap=swap)

    total_triangles <- length(which(dataframe$triangle==TRUE))
    # calcuam probabilitatea pentru formare triunghiuri
    triangle_prob <- get_total_triangles_probability(dataframe, total)

    total_obtuses <- length(which(dataframe$obtuse==TRUE))
    # punctul 4
    # calcuam probabilitatea pentru formare triunghiuri obtuze din toate punctele
    obtuses_prob_from_all_points <- get_total_obtuses_probability_from_all_points(dataframe, total)
    # calcuam probabilitatea pentru formare triunghiuri obtuze doar din triunghiuri
    obtuses_prob_from_triangles <- get_total_obtuses_probability_from_all_triangles(dataframe, total)

    # adaugam titlu pentru plot
    main_text <- paste(
        "(X, Y) @ [0, 1] -> range: ",
        total,
        "\n",
        "total_triangles: ",
        total_triangles,
        " | ",
        triangle_prob, "%",
        "\n",
        "total_obtuses: ",
        total_obtuses,
        " | ",
        obtuses_prob_from_all_points, "%",
        " | ",
        obtuses_prob_from_triangles, "%",
        sep="")

    # punctul 3.a
    size <- 600
    white <- "#ABB2BF"
    # setam culoare de background si foreground pentru plot
    par(bg = "#282C34", fg=white)

    # plotam rezultatele finale
    plot(
        x = dataframe$x,
        y = dataframe$y,
        xlab = "X",
        ylab = "Y",
        xlim = c(0,1.1),
        ylim = c(0,1.1),
        main = main_text,
        col.main = white,
        col.sub = white,       # Subtitle color
        col.lab = white,    # X and Y-axis labels color
        col.axis = white,   # Tick labels color
        col = dataframe$color,
        pch = 16,
        cex = 1.5,
        cex.main = 1.5,   # Title size
        cex.sub = 0.5,  # Subtitle size
        cex.lab = 2,   # X-axis and Y-axis labels size
        cex.axis = 1, # Axis labels size
        # family = "monaco"
    )

    # means for a, b, c
    get_mean_lengths(dataframe)
}

main <- function(total = 5000) {
    # plotam fara swap
    filename_no_swap = paste("plot-no-swap-", total, ".png", sep="")
    # plotam cu swap
    # sa se vada diferenta
    filename_swap = paste("plot-swap-", total, ".png", sep="")
    submain(total, is_triangle_method_1, filename_no_swap, swap = FALSE)
    submain(total, is_triangle_method_1, filename_swap, swap = TRUE)
}



# rulam pentru 5000
main()
