.First <- function() {
require(base)
require(utils)
require(tcltk)

# a helper function...
# Script name: instant_pkgs.R Purpose: Package installation and loading
# Author: Kay Cichini
# Date: 2012-06-19
# Licence: cc by-nc-sa

instant_pkgs <- function(pkgs) {
    pkgs_miss <- pkgs[which(!pkgs %in% installed.packages()[, 1])]
    if (length(pkgs_miss) > 0) {
        install.packages(pkgs_miss)
    }
    
    if (length(pkgs_miss) == 0) {
        message("\n ...Packages were already installed!\n")
    }
    
    # install packages not already loaded:
    pkgs_miss <- pkgs[which(!pkgs %in% installed.packages()[, 1])]
    if (length(pkgs_miss) > 0) {
        install.packages(pkgs_miss)
    }
    
    # load packages not already loaded:
    attached <- search()
    attached_pkgs <- attached[grepl("package", attached)]
    need_to_attach <- pkgs[which(!pkgs %in% gsub("package:", "", attached_pkgs))]
    
    if (length(need_to_attach) > 0) {
        for (i in 1:length(need_to_attach)) require(need_to_attach[i], character.only = TRUE)
    }
    
    if (length(need_to_attach) == 0) {
        message("\n ...Packages were already loaded!\n")
    }
}


# WEBSCRAPE TIRIS AERIAL PHOTOGRAPHY North Tyrol 1940 - 2010 args: X Y
# Coordinates in GK MS28 M Scale height / width aspect will be 1.7 Output HTML
# Animation saved to user's default home directory (!) With a new function-call
# this directory will be overwritten (!)  Temporary Files will be saved to
# default tmp-folder

TIRIS.AERIAL.SEQ <- function(X, Y, M) {
    
    outdir <- path.expand("~\\TIRIS_AERIAL_SEQ_OUTPUT")
    unlink(outdir, recursive = TRUE)
    
    # extent:
    
    H2.1000 <- 66.25  # half heigth at 1:1000
    W2.1000 <- 113  # half width at 1:1000
    
    H2.n <- H2.1000 * M/1000  # Half height at given scale
    W2.n <- W2.1000 * M/1000  # ...  width ...
    
    Left <- ceiling(X - W2.n)
    Bottom <- ceiling(Y - H2.n)
    Right <- floor(X + W2.n)
    Top <- floor(Y + H2.n)
    
    # orthophoto parameter defining the year of record
    
    op_par <- 0:20
    
    df <- cbind(op_par = op_par, Photo_Description = c("0 = aktuelle OP 0000", "1 = IBK-Zirl 2010 digital", 
        "2 = IBK-Schwaz 2008 RGB", "3 = IBK-Schwaz 2008 CIK", "4 = Innsbruck digital 2007", 
        "5 = IBK 1940 1:5.000", "6 = IBK 1940 1:2.500", "7 = ..50-1954", "8 = ..70-1974", 
        "9 = Ötztal 1987", "10 = Lechtal 1992", "11 = Orthofotos 1999", "12 = Orthofotos 2000", 
        "13 = Orthofotos 2001", "14 = Orthofotos 2002", "15 = Orthofotos 2003", "16 = Orthofotos 2004", 
        "17 = Orthofotos 2005", "18 = Orthofotos 2006", "19 = Orthofotos 2007", "20 = Hochwasser 2005"))
    
    # I'll dismiss some of these:
    
    df <- df[-c(1, 4, 7, 21), ]
    
    # then order by years and add id:
    
    df <- cbind(df, order_time = str_extract(df[, "Photo_Description"], "[0-9]{4}"))
    df <- df[order(df[, "order_time"]), ]
    df <- cbind(id = 1:nrow(df), df)
    
    # building df with urls like: url <-
    # 'http://gis.tirol.gv.at/scripts/esrimap.dll?Name=Ortho&MyAufl=1024&MapIDX=1&PktX=80906&PktY=237907&Left=80753&Bottom=237816&Right=81060&Top=237996&Cmd=Pan&AppPar=0&ChkB=0&Mst=1354&PrintMap.x=9&PrintMap.y=15&PrintMap=Druckvorschau'
    # with variables X, Y, Left, Right, Bottom, Top and op_par:
    
    df <- cbind(df, url = rep(NA, nrow(df)))
    
    for (i in 1:nrow(df)) {
        df[i, "url"] <- paste("http://gis.tirol.gv.at/scripts/esrimap.dll?Name=Ortho&MyAufl=1024&MapIDX=1&PktX=", 
            X, "&PktY=", Y, "&Left=", Left, "&Bottom=", Bottom, "&Right=", Right, 
            "&Top=", Top, "&Cmd=Pan&AppPar=", df[i, "op_par"], "&ChkB=0&Mst=", M, 
            "&PrintMap.x=9&PrintMap.y=15&PrintMap=Druckvorschau", sep = "")
    }
    
    # read html of each url making and paste to vector one element for each page's
    # html-code.  the resulting vector will be cbinded to df:
    
    df <- cbind(df, html_strs = rep(NA, nrow(df)))
    
    for (i in 1:nrow(df)) {
        df[i, "html_strs"] <- paste(readLines(df[i, "url"]), collapse = "\n")
    }
    
    # pull img src for each url and download jpegs to tmp-dir indexing with k thru
    # the image sources:
    
    # create new dir in tmp-file folder for holding temporarily produced jpgs.
    # first i'll delete files in tmp-dir, in case there are some:
    
    tmp_dir <- tempdir()
    if (length(dir(tmp_dir)) > 0) {
        file.remove(paste(tmp_dir, dir(tmp_dir), sep = "\\"))
    }
    
    # produce files in loop - first find the image source within the html, then
    # paste missing parts to the url, and finally download to a predefined
    # destination:
    
    for (i in 1:nrow(df)) {
        match_img_src <- str_match_all(df[i, "html_strs"], "<IMG SRC=\"(.*?)\" border")[[1]][2]
        img_src_url <- paste("http://gis.tirol.gv.at", match_img_src, sep = "")
        dest <- paste(tmp_dir, "\\", i, "_Img", ".jpg", sep = "")
        download.file(img_src_url, dest, mode = "wb")
    }
    
    # collect all files from mydir:
    files <- dir(tmp_dir)
    
    # not all arial photos are available for a given location this can be learned
    # from the filesizes. if the size is smaller than max(size)/2.5 it will be
    # deleted
    
    size <- file.info(dir(tmp_dir, full.names = T))$size
    discard <- size < max(size)/2.5
    dir_rm_files <- paste(tmp_dir, "\\", files[discard], sep = "")
    
    file.remove(dir_rm_files)
    files <- dir(tmp_dir)
    
    # sorting of names may not be sequential as we want..  so for that case i'll
    # extract pic-numbers and sort pics accordingly:
    
    kept_ids <- as.integer(sub("_.*", "", files))
    files <- files[order(kept_ids)]
    
    
    # produce HTML: margins and plot axes will be plotted for illustration of the
    # img-positioning within the plot region: fetch the image and align at x0, y0,
    # x1, y1 plot coordinates:
    
    # get serious, set dir-location for output & plot: 858 & 503 is original width
    # and height in px
    
    setwd(tmp_dir)
    
    saveHTML({
        for (i in 1:length(files)) {
            tmp <- readJPEG(files[i])
            par(bg = "grey", mar = c(rep(0, 4)))
            plot(c(0, 858), c(0, 503), type = "n", bty = "n", xlab = "", ylab = "", 
                yaxs = "i", xaxs = "i")
            rasterImage(tmp, 0, 0, 858, 503)
            legend(858 * 0.5, 503 * 0.9, legend = df[sort(kept_ids)[i], "Photo_Description"], 
                bty = "n", text.col = "white", cex = 2, yjust = 0, xjust = 0.5)
        }
    }, ani.height = 503, ani.width = 858, img.name = "pic", interval = 2.5, htmlfile = "TIRIS_AERIAL_SEQ_OUTPUT.html", 
        outdir = outdir, title = "Demo", autobrowse = TRUE, verbose = F, htmlfile = "TIRIS Sequenz.html")
    
    # remove temporarily created files:
    file.remove(dir(getwd()))
    
    graphics.off()
}

# ******************************************************
# the widget...
# ******************************************************
# mydialog <- function() {
    xvar <- tclVar("")
    yvar <- tclVar("")
    zvar <- tclVar("")
    
    tt <- tktoplevel()
    tkwm.title(tt, "TIRIS Luftbild Sequenz")
    x.entry <- tkentry(tt, textvariable = xvar)
    y.entry <- tkentry(tt, textvariable = yvar)
    z.entry <- tkentry(tt, textvariable = zvar)
    
    reset <- function() {
        tclvalue(xvar) <- ""
        tclvalue(yvar) <- ""
        tclvalue(zvar) <- ""
    }
    
    reset.but <- tkbutton(tt, text = "Neueingabe", command = reset)
    
    submit <- function() {
        x <- as.numeric(tclvalue(xvar))
        y <- as.numeric(tclvalue(yvar))
        z <- as.numeric(tclvalue(zvar))
        instant_pkgs(c("animation", "jpeg", "stringr", "ReadImages"))
        TIRIS.AERIAL.SEQ(X = x, Y = y, M = z)
        tkmessageBox(message = "Die fertige Sequenz wird in ihrem Standard-Browser aufgerufen..")
		browseURL(path.expand("~\\TIRIS_AERIAL_SEQ_OUTPUT"))
    }
	
    submit.but <- tkbutton(tt, text = "Bestätigen", command = submit)

    quit.but <- tkbutton(tt, text = "Schließen", 
                 command = function() {
                 q(save = "no")
                 tkdestroy(tt)
                 })
    
    tkgrid(tklabel(tt, text = ""), columnspan = 3)
    tklabel(tt, text = "Bitte Koordinaten und Maßstab eingeben")
    tkgrid(tklabel(tt, text = "X-Koordinate GK M28"), x.entry, padx = 10, pady = 10)
    tkgrid(tklabel(tt, text = "Y-Koordinate GK M28"), y.entry, padx = 10, pady = 10)
    tkgrid(tklabel(tt, text = "Maßstab 1:M"), z.entry, padx = 20, pady = 20)
    tkgrid(reset.but, submit.but, quit.but, padx = 20, pady = 20)
	
# } #..mydialog ends
# ******************************************************
}

save.image("C:/Users/Kay/Dropbox/R-Programs/TIRIS_Scraper/.RData")