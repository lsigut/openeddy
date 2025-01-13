# openeddy 0.0.0.9008

-   precheck_vars object included
-   combine_docu() was removed from openeddy and moved to workflow
    utilities
-   fix bug: NA instead of 9 in extract_coded() for "attangle" and
    "nonsteady"
-   check_manually() now displays also two additional meteo variables
    (requested by Milan and useful when deciding about outliers)
-   apply_QC() was added: removes flux values with QC flag = 2
-   agg_fun() added: you can use arbitrary function for data
    aggregations (e.g. extracting daily temperature minimum, maximum,
    etc.)
-   added filter for LI7200 and LI7500 signal strength in extract_QC()
-   added humidity filter in extract_QC()
-   fix bug: correct logic of messages and order of names in
    choose_avail()
-   add essential_vars_QC object
-   added flag_periods: you can define periods that should be flagged
    (e.g. due to maintenance)
-   bug fix in check_manually(): this previously led to incorrect
    ordering of columns in output files (now in user defined order)
-   bug fix in the computation of spatio-temporal sampling coverage:
    area integration is now correct so values will slightly differ from
    previous version
-   report saving file in check_manually()
-   bug fix: compute correct area under curve in calc_spti_cov()
-   flag_runs() amended for better performance
-   bug fix: ex() was dropping class to list in some cases
-   ggplot_stats() bugfix: stop on too many repeated values
-   bug fix: QC issues in remap_vars()

# openeddy 0.0.0.9007

-   add apply_QC()
-   extend capabilities of extract_coded()
-   generalize extraction of filters and add new filters using
    extract_QC()
-   implement soft flags in apply_thr() for 'between' and 'outside'
-   add circular scale support (angles) to apply_thr()
-   generalize agg_mean() to agg_fun() supporting any function for
    aggregation

# openeddy 0.0.0.9006

-   'shift.by' included in check_manually()
-   allow missing precipitation in plot_eddy() and include examples
-   allow to set maximum range of automatically computed y-axis limits
    with 'ylim' in plot_eddy()
-   add 'ylim' to ggplot_stats() with option to zoom on uncertainty band
-   support two auxiliary vars in exclude() and accordingly in
    check_manually()

# openeddy 0.0.0.9005

-   added read_MeteoDBS(), read_EddyPro(), combine_docu(),
    strip_suffix(), choose_avail()
-   added check_manually() that extends exclude() capabilities
-   added plot_precheck() and plot_hh() for plotting of half-hourly data
-   aggregation functions report 'days' column, i.e. amount of days in
    the aggregation period
-   added barplot_agg() for plotting of aggregated data
-   added ggplot_stats() for exploring wind direction dependencies in
    data
-   added round_df() for rounding double types across data frame
-   produce integers with extract_coded()
-   strptime_eddy() output is forced to be of type integer to escape
    rounding with round_df()
-   support aggregation of evapotranspiration including unit conversion

# openeddy 0.0.0.9004

-   git versioning system adopted for openeddy
-   all changes can be now tracked through the git history
-   time format support for REddyProc online tool
-   assignment of attributes varnames and units was simplified
-   extract_QC(): separated wresid settings for double and planar fit
-   implemented flag = "between" (and "outside") in apply_thr()
-   flag_runs(), structure_eddy(), remap_vars(), ex(), merge_eddy() and
    correct() included
-   changed default values for despikeLF() and strptime_eddy()
-   despikeLF arg 'flux' renamed to 'var'; iterative despiking
    implemented
-   NAs in plot_eddy test arg shown as flag 2
-   function exclude() implemented - interactive manual checking
-   automated recognition of additive filters and NA handling in
    combn_QC() and summary_QC()
-   custom plot panels using modules in plot_eddy()
-   aggregation functions added
-   allow gaps in strptime_eddy()
-   add functions to compute Griebel et al. (2020) budgets with
    uncertainty
-   add function for assessment of spatio_temporal sampling coverage

# openeddy 0.0.0.9003

-   the code is identical with eddyczechr 0.0.0.9003
-   updated package description and requirements
