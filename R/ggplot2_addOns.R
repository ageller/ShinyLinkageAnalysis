# used to set unique axis limits for facets
# this code comes from 
# https://www.zachburchill.ml/ggplot_facets/
# https://stackoverflow.com/questions/63550588/ggplot2coord-cartesian-on-facets

UniquePanelCoords <- ggplot2::ggproto(
	"UniquePanelCoords", ggplot2::CoordCartesian,
	
	num_of_panels = 1,
	panel_counter = 1,
	panel_ranges = NULL,
	
	setup_layout = function(self, layout, params) {
		self$num_of_panels <- length(unique(layout$PANEL))
		self$panel_counter <- 1
		layout
	},
	
	setup_panel_params =  function(self, scale_x, scale_y, params = list()) {
		if (!is.null(self$panel_ranges) & length(self$panel_ranges) != self$num_of_panels)
			stop("Number of panel ranges does not equal the number supplied")
		
		train_cartesian <- function(scale, limits, name, given_range = NULL) {
			if (is.null(given_range)) {
				expansion <- ggplot2:::default_expansion(scale, expand = self$expand)
				range <- ggplot2:::expand_limits_scale(scale, expansion, coord_limits = self$limits[[name]])
			} else {
				range <- given_range
			}
			
			out <- list(
				ggplot2:::view_scale_primary(scale, limits, range),
				sec = ggplot2:::view_scale_secondary(scale, limits, range),
				arrange = scale$axis_order(),
				range = range
			)
			names(out) <- c(name, paste0(name, ".", names(out)[-1]))
			out
		}
		
		cur_panel_ranges <- self$panel_ranges[[self$panel_counter]]
		if (self$panel_counter < self$num_of_panels)
			self$panel_counter <- self$panel_counter + 1
		else
			self$panel_counter <- 1
		
		c(train_cartesian(scale_x, self$limits$x, "x", cur_panel_ranges$x),
			train_cartesian(scale_y, self$limits$y, "y", cur_panel_ranges$y))
	}
)

coord_panel_ranges <- function(panel_ranges, expand = TRUE, default = FALSE, clip = "on") 
{
	ggplot2::ggproto(NULL, UniquePanelCoords, panel_ranges = panel_ranges, 
					expand = expand, default = default, clip = clip)
}