#' Creating table outputs by selection
#'
#' @importFrom stats qt
#' @import gt
#' @import magrittr
#'
#'
#' @param sampH0 The result of samplesH0 function
#' @param sampH1 The result of samplesH1 function
#' @param sampEmp The result of samplesEmp function
#' @param sampEmp0 The result of samplesEmp0 function
#' @param samp30 The result of samples30 function
#' @param select The selection of table options (default is 1 for Proportion of correct positive results
#' (prior to data collection))
#'
#' @return Table wrap-up function of RPS. Based on the selection, different tables
#' are generated;
#' select = 1: Proportion of correct positive results (prior to data collection)
#' select = 2: Proportion of false positive results (prior to data collection)
#' select = 3: Proportion of correct positive results (post-hoc)
#' select = 4: Proportion of false positive results (post-hoc)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' table_RPS(sampH0 = sampH0, sampH1 = sampH1, sampEmp = sampEmp, sampEmp0 = sampEmp0,
#' samp30 = samp30, select = 1)
#' }

table_RPS <- function(sampH0 = sampH0, sampH1 = sampH1, sampEmp = sampEmp, sampEmp0 = sampEmp0,
                      samp30 = samp30, select = 1) {
  # Checking for select value
  # if (is.null(select) == TRUE) {
  #   write("Please make a selection for table outputs")
  # }

  # For table 1
  if (select == 1) {
    Table <- Table1(sampH1 = sampH1, samp30 = samp30)
  }
  # For Figure 2
  if (select == 2) {
    Table <- Table2(sampEmp0 = sampEmp0, sampEmp = sampEmp)
  }
  # For Figure 3
  if (select == 3) {
    Table <- Table3(sampEmp = sampEmp)
  }
  # For Figure 4
  if (select == 4) {
    Table <- Table4(sampH0 = sampH0)
  }

  return(Table)
}
