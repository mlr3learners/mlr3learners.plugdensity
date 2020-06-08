#' @title Density Plug-In Kernel Learner
#'
#' @name mlr_learners_dens.plug
#'
#' @description
#' A [mlr3proba::LearnerDens] implementing plug-in KDE from package
#'   \CRANpkg{plugdensity}.
#' Calls [plugdensity::plugin.density()].
#'
#' @templateVar id dens.plug
#' @template section_dictionary_learner
#'
#' @references
#' J. Engel, Eva Herrmann and Theo Gasser (1994).
#' An iterative bandwidth selector for kernel estimation of densities and their derivatives.
#' Journal of Nonparametric Statistics 4, 21–34.
#'
#' @template seealso_learner
#' @template example
#' @export
LearnerDensPlugin = R6Class("LearnerDensPlugin",
  inherit = LearnerDens,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(
        params = list(
          ParamLgl$new(id = "na.rm", default = FALSE, tags = "train")
        )
      )

      super$initialize(
        id = "dens.plug",
        packages = "plugdensity",
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        predict_types = "pdf",
        param_set = ps,
        properties = "missings",
        man = "mlr3learners.plugdensity::mlr_learners_dens.plug"
      )
    }
  ),

  private = list(
    .train = function(task) {
      pdf <- function(x1) {} # nolint
      body(pdf) <- substitute({
        mlr3misc::invoke(plugdensity::plugin.density, x = data, xout = x1, na.rm = TRUE)$y
      }, list(data = task$truth()))

      distr6::Distribution$new(
        name = "Plugin KDE",
        short_name = "PluginKDE",
        pdf = pdf)
    },

    .predict = function(task) {
      mlr3proba::PredictionDens$new(task = task, pdf = self$model$pdf(task$truth()))
    }
  )
)
