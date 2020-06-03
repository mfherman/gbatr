#' Geocode address using GBAT
#'
#' @description Geocode a data frame of addresses using the NYC Department of
#'   City Planning's Geosupport Desktop Application.
#'
#' @param df An input data frame to geocode
#' @param address Name of street address column in input data frame
#' @param zip_boro Name of zip code or borough code column in input data frame.
#'   For convinience, borough codes in the input data can be borough codes
#'   (`"1"`, `"2"`, `"3"`, `"4"`, `"5"`), borough names (`"Manhattan"`,
#'   `"Bronx"`, `"Brooklyn"`, `"Queens"`, `"Staten Island"`), county names
#'   (`"New York"`, `"Bronx"`, `"Kings"`, `"Queens"`, `"Richmond"`), or a mix of
#'   the three types.
#' @param zip_boro_type The `zip_boro` column type in the input data frame.
#'   Either `"zip"` or `"boro"`, defaults to `"zip"`.
#' @param geo_colnames A character vector of column names to return from the
#'   GBAT geocoder and/or one or more GBAT "presets." See below for details. If
#'   `NULL` (the default), all GBAT fields are returned.
#' @param func GBAT output function columns to return. One or more of `"F1A"`,
#'   `"F1E"`, and `"FAP"`. Defaults to all three functions.
#' @param append Whether or not to append the GBAT geocoder output to the input
#'   data frame or just return the geocoder output. Defaults to `TRUE`.
#'
#' @return A data.frame (or [tibble][tibble::tibble-package]) with GBAT geocoder
#'   output
#'
#' @details To make it easier to return certain common fields from the GBAT
#'   geocoder, there are a handful of "short cuts" you can use in the
#'   `geo_colnames` argument to return certain fields.
#'
#'   * `"lot_centrold_xy"` -- `"F1A_Xcoordinate"`, `"F1A_Ycoordinate"`
#'   * `"lot_centroid_latlon"` -- `"F1A_Latitudee"`, `"F1A_Longitude"`
#'   * `"block_face_xy"` -- `"F1E_XCoordinate"`, `"F1E_YCoordinate"`
#'   * `"block_face_latlon"` -- `"F1E_Latitude"`, `"F1E_Longitude"`
#'   * `"cd"` -- `"F1E_CommunityDistrict"`
#'   * `"nta"` -- `"F1E_NTA"`, `"F1E_NTAName"`
#'   * `"census_tract"` -- `"F1E_2010CensusTractGEOID"`
#'   * `"census_block"` -- `"F1E_2010CensusBlockGEOID"`
#'
#' @export
#'
#' @examples
#'
#' df <- data.frame(
#'   address = c("261 Moore Street", "1524 Neptune Avenue"),
#'   zip_code = c("11206", "11224")
#'   )
#'
#' gbat(df, "address", "zip_code", "zip", geo_colnames = "lot_centroid_latlon")

gbat <- function(df, address, zip_boro, zip_boro_type = c("zip", "boro"),
                 func = c("F1A", "F1E", "FAP"), geo_colnames = NULL,
                 append = TRUE) {

  # check that input is data frame
  if (!inherits(df, "data.frame")) {
    stop("You must supply a data frame as input to be geocoded", call. = FALSE)
  }

  # check that address and zip or boro column name supplied is in input df
  input_cols <- c(address, zip_boro)
  if (!all(input_cols %in% names(df))) {
    stop("`address` and `zip_boro` column names not found in input data frame",
         call. = FALSE)
  }

  # validate zip_boro_type argument
  zip_boro_type <- match.arg(zip_boro_type)
  zip_boro_type <- switch(zip_boro_type,
                          zip = "zip_code",
                          boro = "borough_code")

  # validate func argument
  func <- match.arg(func, several.ok = TRUE)

  # validate column names to return from gbat
  short_cuts <- list(
    lot_centroid_xy      = c("F1A_Xcoordinate", "F1A_Ycoordinate"),
    lot_centroid_latlon  = c("F1A_Latitude", "F1A_Longitude"),
    block_face_xy        = c("F1E_XCoordinate", "F1E_YCoordinate"),
    block_face_latlon    = c("F1E_Latitude", "F1E_Longitude"),
    cd                   = "F1E_CommunityDistrict",
    nta                  = c("F1E_NTA", "F1E_NTAName"),
    census_tract         = c("F1E_2010CensusTractGEOID"),
    census_block         = c("F1E_2010CensusBlockGEOID")
    )

  if (!all(geo_colnames %in% c(gbatr::gbat_fields$col_name, names(short_cuts)))) {
    stop("Invalid column name selection. All names must match one of `gbat_fields$col_name` or one of the short cut names",
         call. = FALSE)
  }

  # make a copy of input data because we may need to use the input data frame at the end
  # TODO: will this slow things down too much? probably could refactor so this isn't necessary
  to_geo <- df

  # because we're helpful, convert borough codes, county names, or borough names
  # to correct borough codes for GBAT
  if (zip_boro_type == "borough_code") {
    to_geo[[zip_boro]] <- tolower(to_geo[[zip_boro]])
    to_geo[[zip_boro]] <- ifelse(to_geo[[zip_boro]] %in% c("1", "new york", "manhattan", "man", "ny"), "1",
                            ifelse(to_geo[[zip_boro]] %in% c("2", "bronx", "bx"), "2",
                              ifelse(to_geo[[zip_boro]] %in% c("3", "kings", "brooklyn", "bk"), "3",
                                ifelse(to_geo[[zip_boro]] %in% c("4", "queens", "qn", "qns"), "4",
                                  ifelse(to_geo[[zip_boro]] %in% c("5", "richmond", "staten island", "si"), "5", "0")))))
  }

  # add rownumber as id col for GBAT
  to_geo$.id772018 <- seq.int(nrow(to_geo))

  # run gbat!!!
  # TODO: is it faster if we specify which columns to be returned in the cpp code?
  # may be important if data frame becomes to big for memory
  gbat_out <- GBAT(
    to_geo,
    id_col = ".id772018",
    add_col = address,
    third_col = zip_boro,
    third_col_type = zip_boro_type
    )

  # some special handling in parsing the census tract and block vars
  census_vars <- c("1990CensusTract", "2000CensusTract", "2010CensusTract",
                   "2010CensusBlock", "2000CensusBlock")

  # parse gbat output from single fixed width columns in one col per field
  # TODO clean this up!!
  sapply(1:sum(!is.na(gbatr::gbat_fields$field)), function(i)

    if (gbatr::gbat_fields$field[i] %in% census_vars) {

      gbat_out[[paste0(gbatr::gbat_fields$func[i], "_", gbatr::gbat_fields$field[i])]] <<-
        gsub(
          "\\s", "0",
          substr(
            gbat_out[[paste0(gbatr::gbat_fields$func[i],"_output")]],
            gbatr::gbat_fields$start[i], gbatr::gbat_fields$end[i]
            ),
          perl = TRUE
          )

      } else {

        gbat_out[[paste0(gbatr::gbat_fields$func[i], "_", gbatr::gbat_fields$field[i])]] <<-
          gsub(
            "(?<=[\\s])\\s*|^\\s+|\\s+$", "",
            substr(gbat_out[[paste0(gbatr::gbat_fields$func[i],"_output")]],
                   gbatr::gbat_fields$start[i], gbatr::gbat_fields$end[i]
                   ),
            perl = TRUE
            )
        }
    )

  # remove gbat output columns and id
  gbat_out <- gbat_out[, !names(gbat_out) %in% c("F1A_output", "F1E_output", "FAP_output", ".id772018")]

  # change all blanks coming out of geocoder to na
  gbat_out[gbat_out == ""] <- NA

  # TODO once we fix the census tract parsing above, this won't be necessary
  gbat_out[gbat_out == "0000"] <- NA
  gbat_out[gbat_out == "000000"] <- NA

  # create new census cols with geoids that will match census files
  # replace borough codes with borough geoids
  gbat_out[["F1E_CensusCountyGEOID"]] <- ifelse(gbat_out[["F1E_CensusBoro"]] == 1, "36061",
                                         ifelse(gbat_out[["F1E_CensusBoro"]] == 2, "36005",
                                         ifelse(gbat_out[["F1E_CensusBoro"]] == 3, "36047",
                                         ifelse(gbat_out[["F1E_CensusBoro"]] == 4, "36081",
                                         ifelse(gbat_out[["F1E_CensusBoro"]] == 5, "36085",
                                                NA)))))

  # combine county and tract to create geoid
  gbat_out[["F1E_1990CensusTractGEOID"]] <- ifelse(!is.na(gbat_out[["F1E_1990CensusTract"]]),
                                                   paste0(gbat_out[["F1E_CensusCountyGEOID"]],
                                                          gbat_out[["F1E_1990CensusTract"]]),
                                                   NA)

  gbat_out[["F1E_2000CensusTractGEOID"]] <- ifelse(!is.na(gbat_out[["F1E_2000CensusTract"]]),
                                                   paste0(gbat_out[["F1E_CensusCountyGEOID"]],
                                                          gbat_out[["F1E_2000CensusTract"]]),
                                                   NA)

  gbat_out[["F1E_2010CensusTractGEOID"]] <- ifelse(!is.na(gbat_out[["F1E_2010CensusTract"]]),
                                                   paste0(gbat_out[["F1E_CensusCountyGEOID"]],
                                                          gbat_out[["F1E_2010CensusTract"]]),
                                                   NA)

  # combine tract, block, block suffix to create geoid
  gbat_out[["F1E_2000CensusBlockGEOID"]] <- ifelse(is.na(gbat_out[["F1E_2000CensusBlock"]]), NA,
                                            ifelse(!is.na(gbat_out[["F1E_2000CensusBlockSuffix"]]),
                                                   paste0(gbat_out[["F1E_2000CensusTractGEOID"]],
                                                          gbat_out[["F1E_2000CensusBlock"]],
                                                          gbat_out[["F1E_2000CensusBlockSuffix"]]),
                                                   paste0(gbat_out[["F1E_2000CensusTractGEOID"]],
                                                          gbat_out[["F1E_2000CensusBlock"]])))

  gbat_out[["F1E_2010CensusBlockGEOID"]] <- ifelse(is.na(gbat_out[["F1E_2010CensusBlock"]]), NA,
                                            ifelse(!is.na(gbat_out[["F1E_2010CensusBlockSuffix"]]),
                                                   paste0(gbat_out[["F1E_2010CensusTractGEOID"]],
                                                          gbat_out[["F1E_2010CensusBlock"]],
                                                          gbat_out[["F1E_2010CensusBlockSuffix"]]),
                                                   paste0(gbat_out[["F1E_2010CensusTractGEOID"]],
                                                          gbat_out[["F1E_2010CensusBlock"]])))

  # if func argument is not all 3, subset gbat output to cols that start with
  # specified function
  if (length(func < 3)) {
    func <- paste(func, collapse = "|")
    gbat_out <- gbat_out[, names(gbat_out) %in% c(input_cols, "F1E_GRC",
                                                  names(gbat_out)[grepl(func, names(gbat_out))])]
  }

  # if colnames are specified subset gbat output to return those matching cols
  if (!is.null(geo_colnames)) {

    # first we'll check if any of the short cuts were used
    # if so use the named list to extract the colnames and add to the geo_colnames vector
    if (any(geo_colnames %in% names(short_cuts))) {
      nms <- geo_colnames[geo_colnames %in% names(short_cuts)]
      cols <- unlist(short_cuts[names(short_cuts) %in% nms], use.names = FALSE)
      geo_colnames <- c(geo_colnames[!geo_colnames %in% names(short_cuts)], cols)
    }
    # and now we'll keep only address/zip/boro and all the geo_colnames specified
    gbat_out <- gbat_out[, names(gbat_out) %in% c(input_cols, "F1E_GRC", geo_colnames)]
  }

  # should we append the geocoder output to the input data frame?
  # if not just address, borough/zip and geo out cols are returned
  # TODO: confirm that row order doesn't change with geocoder output or parsing
  if (append) {
    # move return code column to directly after input cols
    gbat_out <- gbat_out[, c(input_cols, "F1E_GRC", names(gbat_out)[!names(gbat_out) %in% c(input_cols, "F1E_GRC")])]

    gbat_out <- cbind(df, gbat_out[, !names(gbat_out) %in% input_cols, drop = FALSE])

  } else {
    # replace these two output columns from gbat with cols from input data
    gbat_out[[address]] <- df[[address]]
    gbat_out[[zip_boro]] <- df[[zip_boro]]

    # move return code column to directly after input cols
    gbat_out <- gbat_out[, c(input_cols, "F1E_GRC", names(gbat_out)[!names(gbat_out) %in% c(input_cols, "F1E_GRC")])]
  }

  # make it a tibble if the package is installed and input data was a tibble
  if (requireNamespace("tibble", quietly = TRUE) && inherits(df, "tbl_df")) {
    gbat_out <- tibble::as_tibble(gbat_out)
  }

  gbat_out

}
