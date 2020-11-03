#' CoNLL class.
#' 
#' Toolset for a workflow to reshape a CoNLL input to a CWB compatible format.
#' 
#' @examples
#' if (FALSE){
#'   use("GermaParl")
#'   filename <- system.file(package = "maxqda", "extdata", "CoNLL", "16_126_FDP_Markus_Loening_2871305.tsv")
#'   P <- partition("GERMAPARL", text_lp = "16", text_protocol_no = "126", text_speaker = "Markus Loening")
#'   Anno <- CoNLL$new(filename = filename, partition = P)
#'   Anno$getCorpusPositions()
#'   annotations <- Anno$cpos
#'   cpos <- unlist(apply(annotations[id == "B-15_15"], 1, function(row) row["cpos_left"]:row["cpos_right"]))
#'   html(P) %>% highlight(highlight = list(yellow = cpos)) %>% htmltools::html_print()
#' }
#' @export CoNLL
#' @importFrom data.table rbindlist setkeyv setcolorder
#' @importFrom htmltools html_print
CoNLL <- R6Class(
  
  "CoNLL",
  
  public = list(
    
    #' @field tab STORY TO BE TOLD
    tab = NULL,
    #' @field filename character string, the filename of the CoNLL annotation
    filename = NULL, # a character string
    
    #' @field partition a partition object, generated on the basis of sAttributes
    
    partition = NULL, # partition
    
    #' @field cpos a data.table with the columns "cpos_left" and "cpos_right" defining the regions of the annotations
    cpos = NULL,
    
    #' @param filename name of file to be processed
    #' @param partition partition where to locate annotations
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
    
    #' @description
    #' STORY TO BE TOLD
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