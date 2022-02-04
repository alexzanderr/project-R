


round_decimal <- function(x, digits, chars = TRUE) {
    if (grepl(x = x, pattern = "\\.")) {
        y <- as.character(x)
        pos <- grep(unlist(strsplit(x = y, split = "")), pattern = "\\.", value = FALSE)
        if (chars) {
            return(
                as.double(substr(x = x, start = 1, stop = pos + digits))
            )
        }
        return(
            as.double(substr(x = x, start = 1, stop = pos + digits))
        )
    } else {
        return(
            as.double(format(round(x, 2), nsmall = 2))
        )
    }
}




obtuses <- 0
is_obtuse_triangle <- function(a, b, c) {
    values <- c(a, b, c)
    first_min <- min(values)
    filtered <- list()

    max_ = max(values)
    second_min <- NULL
    for (value in values) {
        if (value == first_min) {
            next
        }
        filtered <- append(filtered, value)
        # print(value)
    }
    if (length(filtered) == 1 || length(filtered) == 0) {
        second_min <- first_min
    } else if (length(filtered) == 2) {
        second_min <- min(unlist(filtered))
    }
    # print(first_min)
    # print(second_min)
    # print(max_)

    values <- c(first_min, second_min, max_) ^ 2
    # suma celor mai mici mai mica decat cea mai mare
    suma_minime <- values[1] + values[2]

    if (suma_minime < values[3]) {
        obtuses <<- obtuses + 1
        # print(values)
        # cat(" min=", first_min, "second_min=", second_min, "max=", max_, "\n")
        # cat(" min^2 =", values[1], "second_min^2 =", values[2], " < ", "max^2=", values[3], "\n")
        # cat("\n")
        return (TRUE)
    }
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


generate_xz <- function(total = 5000) {
    # 5000 individual unique random values between [0, 1]
    x <- runif(total, 0, 1)
    # 5000 individual unique random values between [0, 1]
    z <- runif(total, 0, 1)
    # why dataframe?
    # its much easier to manage
    return (data.frame(x = x, z = z))
}




generate_dataframe <- function(
    dataframe,
    swap = FALSE,
    is_triangle = is_triangle_method_1
) {
    pairs = list(list(dataframe$x), list(dataframe$z))
    colors = c()
    triangle = c()
    obtuse = c()
    aa <- c()
    bb <- c()
    cc <- c()
    for (iter in seq_len(length(dataframe$z))) {
        # print(iter)
        x <- pairs[[1]][[1]][iter]
        z <- pairs[[2]][[1]][iter]

        if (swap) {
            if (x > z) {
                temp <- x
                x <- z
                z <- temp
            }
        }

        a <- x
        b <- abs(x - z)
        c <- abs(1 - z)

        # create the columns for dataframe
        aa <- append(aa, a)
        bb <- append(bb, b)
        cc <- append(cc, c)

        # check if the a, b, c lengths form a triangle
        if (is_triangle(a, b, c)) {
            triangle <- append(triangle, TRUE)

            # check if the a, b, c lengths form an obtuse triangle
            if (is_obtuse_triangle(a, b, c)) {
                obtuse <- append(obtuse, TRUE)
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
    # cat("obtuses=", length(which(obtuse==TRUE)), "\n")
    dataframe[,"a"] <- aa
    dataframe[,"b"] <- bb
    dataframe[,"c"] <- cc
    dataframe[,"color"] <- colors
    dataframe[,"triangle"] <- triangle
    dataframe[,"obtuse"] <- obtuse
    return (dataframe)
}


get_total_triangles_probability <- function(dataframe, total) {
    total_triangles = length(which(dataframe$triangle==TRUE))
    return (total_triangles / total)
}


get_total_obtuses_probability_from_all_points <- function(dataframe, total) {
    total_triangles = length(which(dataframe$obtuse==TRUE))
    return (total_triangles / total)
}
get_total_obtuses_probability_from_all_triangles <- function(dataframe, total) {
    total_triangles = length(which(dataframe$triangle==TRUE))
    total_obtuses = length(which(dataframe$obtuse==TRUE))
    return (total_obtuses / total_triangles)
}

# punctul 2
get_mean_lengths <- function(dataframe) {
    as_mean = paste("all a's mean: ", mean(dataframe$a), sep="")
    # print(as_mean)
    bs_mean = paste("all b's mean: ", mean(dataframe$b), sep="")
    # print(bs_mean)
    cs_mean = paste("all c's mean: ", mean(dataframe$c), sep="")
    # print(cs_mean)

    # individual means for the first 5
    for (i in 1:5) {
        a = dataframe$a[i]
        b = dataframe$b[i]
        c = dataframe$c[i]
        # cat(a, b, c, "\n")

        mean_a_b_c = mean(c(a, b, c))
        # print(mean_a_b_c)
    }
}

submain <- function(total, triangle_func, filename, swap) {
    xz <- generate_xz(total)
    # print(head(xy))
    dataframe <- generate_dataframe(
        xz,
        swap=swap,
        is_triangle = triangle_func)

    total_triangles <- length(which(dataframe$triangle==TRUE))
    triangle_prob <- get_total_triangles_probability(dataframe, total)

    total_obtuses <- length(which(dataframe$obtuse==TRUE))
    # punctul 4
    obtuses_prob_from_all_points <- get_total_obtuses_probability_from_all_points(dataframe, total)
    obtuses_prob_from_triangles <- get_total_obtuses_probability_from_all_triangles(dataframe, total)

    main_text <- paste(
        "(X, Z) @ [0, 1] -> range: ",
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
    png(file = filename,
        width = size,
        height = size)
    # options(repr.plot.width = 500, repr.plot.height = 500)
    par(bg = "#282C34", fg=white)
    # dev.new(width = 1000, height = 1000, unit = "px")
    plot(
        x = dataframe$x,
        y = dataframe$z,
        xlab = "X",
        ylab = "Z",
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
    # print(dev.size("px"))
    # rect(0, 0, 1000, 1000, col = "#282C34", border = NULL)

    # text <- "asiodujbasiodb aiosgbdioasbd"
    # mtext(text)
    # plotted <- recordPlot()
    dev.off()

    # means for a, b, c
    get_mean_lengths(dataframe)


    # punctul 3.b
    triangle_prob_string = paste("probability for triangles: ", triangle_prob, "%", sep="")
    obtuse_prob_string = paste("probability for obtuses from all points: ", obtuses_prob_from_all_points, "%", sep="")
    obtuse_prob_string = paste("probability for obtuses from all triangles: ", obtuses_prob_from_triangles, "%", sep="")
    # print(length(which(dataframe$x==0.33)))
}


main <- function(total = 5000) {
    filename_no_swap = paste("plot-no-swap-Z-", total, ".png", sep="")
    filename_swap = paste("plot-swap-Z-", total, ".png", sep="")
    submain(total, is_triangle_method_1, filename_no_swap, swap = FALSE)
    submain(total, is_triangle_method_1, filename_swap, swap = TRUE)
}



# haha, programare procedurala
# main(100000)
# probability: 0.24949 (asta e cam cea mai accurate)
main()

