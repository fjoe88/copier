#'Copy unique values to clipboard.
#'
#'1) if input is a dataframe, do not return back a df \cr
#'2) by default will assume lotids being copied and format them unless set format_lotid = FALSE \cr
#'3) if format_step == TRUE, return a comma separated step name string instead \cr
#'4) if save_csv == TRUE, will save output to '1.lot_list.csv' \cr
#'
#'@param df a character string copied from clipboard, or a dataframe column
#'@param col column name if df is a data frame
#'@return a comma separated string w/ duplicates removed and a dataframe of unique values
#'
#'@example
#'copy_unique(df = "apple banana apple orange", format_lotid = FALSE)

copy_unique <-
  function(df = paste(readClipboard(), collapse = ","),
           col = "lotid",
           sep = ",",
           quotes = FALSE,
           format_lotid = TRUE,
           format_step = FALSE,
           save_csv = FALSE,
           ...) {

    if (!(is.character(df)|is.data.frame(df)))
      stop("only data.frame objects or character vectors are accepted")

    if (is.data.frame(df)) {
      if (tolower(col) == "lotid") {
        df <- wafer::format_lotid(df)
        df <- wafer::create_startlot(df)
      }
      unique.val <- unique(df[[col]])
      str.to.copy <- paste(unique.val, collapse = sep)
      utils::writeClipboard(str.to.copy)
      return(df)

    } else{

      # early return if to format step names
      if (format_step == TRUE){
        dt <- data.table::setDT(stringr::str_split(df, ","))
        dt <- unique(dt)

        col = "step"
        names(dt) <- col

        if (quotes == TRUE) {
          str <- paste(paste0("'", dt[[col]], "'"), collapse = ",")
        }

        utils::writeClipboard(str)
        return(dt)
      }

      if (length(df) > 1) {
        df = paste(df, collapse = ",")
      }

      # early return if nothing copied
      if (!grepl("[[:alnum:]]", df)) {return()}

      df <-
        stringr::str_replace_all(df, pattern = "\\s+", replacement = ",")
      df <-
        stringr::str_replace_all(df, pattern = "\n", replacement = ",")
      df <-
        stringr::str_replace_all(df, pattern = "^,+|,+$", replacement = "")
      df <-
        stringr::str_replace_all(df, pattern = ",+", replacement = ",")

      dt <- data.table::setDT(stringr::str_split(df, ","))

      names(dt) <- col

      if (format_lotid == TRUE) {
        names(dt) <- "lotid"
        dt <- wafer::format_lotid(dt)
        dt <- wafer::create_startlot(dt)
      }

      if (quotes == TRUE) {
        dt[[col]] <- paste0("'", dt[[col]], "'")
      }

      data.table::setkeyv(dt, col)
      dt <- unique(dt)

      utils::writeClipboard(paste(dt[[col]], collapse = sep))

      if (save_csv == TRUE) {
        d8ahelper::save_csv(dt, file.name = "lot_list.csv")
      }

      return(dt)
    }
  }


