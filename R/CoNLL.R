#' CoNLL class.
#' 
#' Toolset for a workflow to reshape a CoNLL input to a CWB compatible format.
#' 
#' @param filename name of file to be processed
#' @param partition partition where to locate annotations
#' 
#' @field filename character string, the filename of the CoNLL annotation
#' @field cpos a data.table with the columns "cpos_left" and "cpos_right" defining the regions of the annotations
#' @field partition a partition object, generated on the basis of sAttributes
#' @section Methods:
#' \describe{
#'   \item{\code{$new(filename, partition)}}{}
#'   \item{\code{$load()}}{Read the content of the file specified upon initialization by \code{filename},
#'   and keep the parsed input in the slot \code{tab}}
#'   \item{\code{$getCorpusPositions}}{}
#' }
#' @examples
#' if (require("GermaParl")){
#'   use("GermaParl")
#'   filename <- system.file(package = "polmineR.anno", "extdata", "CoNLL", "16_126_FDP_Markus_Löning_2871305.tsv")
#'   P <- partition("GERMAPARL", text_lp = "16", text_protocol_no = "126", text_speaker = "Markus Löning")
#'   Anno <- CoNLL$new(filename = filename, partition = P)
#'   Anno$getCorpusPositions()
#'   annotations <- Anno$cpos
#'   cpos <- unlist(apply(annotations[id == "B-15_15"], 1, function(row) row["cpos_left"]:row["cpos_right"]))
#'   html(P) %>% highlight(highlight = list(yellow = cpos)) %>% htmltools::html_print()
#' }
#' @export CoNLL
#' @importFrom data.table rbindlist setkeyv setcolorder
CoNLL <- R6Class(
  
  "CoNLL",
  
  public = list(
    
    tab = NULL,
    filename = NULL, # a character string
    partition = NULL, # partition
    cpos = NULL,
    
    initialize = function(filename, partition){
      stopifnot(!is.null(filename))
      self$filename <- filename
      tabRaw <- readLines(self$filename)
      self$tab <- do.call(
        rbind,
        lapply(
          strsplit(tabRaw, "\t"),
          function(x){
            c(x[1], paste("|", paste(x[2:length(x)], collapse = "|"), "|", sep = ""))
          })
      )

      self$partition <- partition
    },
    
    load = function(){
    },
    
    getCorpusPositions = function(){
      # create a table with three coumns: "id", "start", "end"
      posList <- lapply(
        grep("B-", self$tab[,2]),
        function(startPosition){
          ids <- strsplit(self$tab[startPosition, 2], "\\|")[[1]]
          ids <- ids[which(sapply(ids, nchar) > 0)]
          posList <- lapply(
            ids,
            function(id){
              followUp <- gsub("^B-", "I-", id)
              endPosition <- startPosition + 1 
              if (endPosition > nrow(self$tab)){
                # case: annotation starts with the last token
                return( data.table(id = id, pos_left = startPosition, pos_right = startPosition) )
              } else if (grepl(followUp, self$tab[endPosition, 2]) == FALSE){
                # case: only one token annotated
                return( data.table(id = id, pos_left = startPosition, pos_right = startPosition) )
              } else {
                while (TRUE){
                  if (grepl(followUp, self$tab[endPosition, 2]) == FALSE){
                    endPosition <- endPosition - 1
                    break
                  } else {
                    endPosition <- endPosition + 1
                  }
                  if (endPosition > nrow(self$tab)){
                    endPosition <- nrow(self$tab)
                    break
                  }
                    
                }
                return( data.table(id = id, pos_left = startPosition, pos_right = endPosition) )
              }
            }
          )
          rbindlist(posList)
        }
      )
      posDT <- rbindlist(posList)
      
      tokensPartition <- getTokenStream(self$partition, pAttribute = "word", cpos = TRUE)
      cposList <- lapply(
        1:nrow(posDT),
        function(i){
          tokensCoNNLL <- self$tab[as.integer(posDT[i, "pos_left", with = FALSE]):as.integer(posDT[i, "pos_right", with = FALSE]), 1]
          potentialStart <- grep(tokensCoNNLL[1], tokensPartition)
          if (length(potentialStart) > 0){
            matchList <- lapply(
              potentialStart,
              function(x){
                y <- x + length(tokensCoNNLL) - 1
                partitionSequenceNamed <- tokensPartition[x:y]
                partitionSequenceCpos <- names(partitionSequenceNamed)
                partitionSequence <- unname(partitionSequenceNamed)
                if (identical(tokensCoNNLL, partitionSequence)){
                  return(
                    data.table(
                      id = as.character(posDT[i, "id", with = FALSE]),
                      pos_left = as.integer(posDT[i, "pos_left", with = FALSE]),
                      pos_right = as.integer(posDT[i,3, "pos_right", with = FALSE]),
                      cpos_left = as.integer(partitionSequenceCpos[1]),
                      cpos_right = as.integer(partitionSequenceCpos[length(partitionSequenceCpos)]),
                      quote = paste(tokensCoNNLL, collapse = " ")
                    )
                  )
                } else {
                  return(NULL)
                }
              }
            )
            if (is.null(matchList)){
              return( NULL )
            } else {
              for (i in rev(which(sapply(matchList, is.null) == TRUE))) matchList[[i]] <- NULL
              return( rbindlist(matchList) )
            }
          } else {
            return( NULL )
          }
        }
      )
      cposDT <- rbindlist(cposList)
      if (nrow(cposDT) == 0) return(NULL)
      setkeyv(cposDT, cols = c("id", "pos_left", "pos_right"))
      setkeyv(posDT, cols = c("id", "pos_left", "pos_right"))
      self$cpos <- cposDT[posDT]
      self$cpos[, "pos_left" := NULL][, "pos_right" := NULL]
      setcolorder(self$cpos, neworder = c("cpos_left", "cpos_right", "id", "quote"))
      invisible(self$cpos)
    }
  )
)