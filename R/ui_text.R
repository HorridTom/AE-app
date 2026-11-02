# Text of the "Understanding the analysis" and "Development" tabs.

understanding_tab_content <- function() {
  tagList(
    h1("Understanding the analysis"),
    p(
      "This application provides statistical analysis of attendance and admission data relating
      to providers of NHS accident and emergency department services in England and Scotland."
    ),
    p(
      "All the data used
    is publicly available from the NHS England and ISD Scotland websites:"
    ),
    a(
      "A&E waiting times and activity for England",
      href = "https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/"
    ),
    p("\n"),
    a(
      "A&E waiting times and activity for Scotland",
      href = "https://www.opendata.nhs.scot/dataset/weekly-accident-and-emergency-activity-and-waiting-times"
    ),
    p("\n"),
    p(
      "Contains public sector information licensed under the ",
      a(
        "Open Government Licence v3.0",
        href = "https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/"
      ),
      ". This analysis is not an official publication of NHS England or Public Health Scotland."
    ),
    p("\n"),
    p(
      "Note that due to the 'rapid care measures' pilot, data for some NHS England providers is not available from May 2019.
      By default this analysis now excludes these providers for the whole time period. If you wish to see the analysis
      for all providers, untick the control box on the right hand panel. The four hour performance measure is not comparable
      before and after the start of the pilot in this case, and so is only displayed up to April 2019."
    ),
    p("\n"),
    a(
      "King's Fund report on the review of A&E standards",
      href = "https://www.kingsfund.org.uk/publications/nhs-england-review-waiting-times-accident-emergency"
    ),
    p("\n"),
    p(
      "Providers are listed under their most recent name. Where a provider's code has been used with other
      names in the past, these are shown above its charts. Where two providers have the same name, their
      codes are shown after the name. Where a service has been reported under a new code (for example,
      when a new provider took it over), the charts for each code are shown separately, with a link
      from each to the other: the service may have changed at the same time, so the data before and
      after are not combined."
    ),
    p("\n"),
    p(
      "Note that NHS Scotland data is provided weekly, and may be analysed either weekly as-is, or attributed
      across months and analysed monthly. Tick/untick the 'Weekly analysis' box to toggle between these
      two analysis modes."
    ),
    p("\n"),
    p(
      "Note that Scottish data for admissions is not currently publically available and therefore admissions charts are not included in the analysis for Scotland."
    ),
    h2("Shewhart Charts"),
    p(
      "The analysis uses Shewhart charts, also known as control charts. There
      is a brief explanation of this approach below, for more information
      please follow the links in the resources section.
      "
    ),
    p(
      "Shewhart charts can help understand whether and how a measure is changing
      over time. The basic components of a Shewhart chart are:"
    ),
    tagAppendAttributes(
      tags$ol(
        tags$li("A measure plotted over time as a line graph"),
        tags$li(
          "A horizontal centre line, often an average, indicating the typical level of the measure"
        ),
        tags$li(
          "Control limits: horizontal lines indicating the range the measure usually stays within"
        )
      ),
      type = 'A'
    ),
    p(
      "To interpret a Shewhart chart, a set of rules is used. The rules indicate when the way the
      measure varies is different in some way from its typical behaviour. The rules we use in this
      analysis are as follows:"
    ),
    tags$ol(
      tags$li(
        "Any month/week outside the control limits (highlighted in orange)"
      ),
      tags$li(
        "Eight or more consecutive months/weeks all above, or all below, the centre line
              (highlighted in blue)"
      )
    ),
    p(
      "Any instance of one of these rules being triggered by a measure indicates that there is likely
      to be an identifiable cause for this particular pattern in the data. This is known as a special cause
      and is worth investigating locally. Learning from special cause variation in a measure can indicate actions
      that will result in improvements in patient care in future."
    ),
    p(
      "If no rules are triggered, this means that the measure is continuing to behave as it usually does. This
      indicates that in order to achieve improvements, there is no point in focussing on individual months' highs
      and lows. The system needs to be locally diagnosed, and improvments implemented based on the results."
    ),
    h2("Resources"),
    p("An introduction to control charts in healthcare:"),
    a(
      "Statistical process control as a tool for research and healthcare improvement",
      href = "http://dx.doi.org/10.1136/qhc.12.6.458"
    ),
    br(),
    p("Practical, interactive guide to using data to drive improvement:"),
    a(
      "NHS Improvement: Making Data Count",
      href = "https://improvement.nhs.uk/resources/making-data-count/"
    ),
    br(),
    p(
      "This analysis uses p-prime and u-prime charts, more information
      is available here:"
    ),
    a(
      "Prime charts publication",
      href = "http://dx.doi.org/10.1136/qshc.2006.017830"
    )
  )
}

development_tab_content <- function() {
  tagList(
    h1("Development"),
    p(
      "This website was developed by the NIHR CLAHRC Northwest London
      Public Health and Information Intelligence Theme in collaboration with
       NHS England London. It is in a beta-testing phase at present, meaning
      that we will review and make further improvements to the site on a regular
      basis."
    ),
    p(
      "The code for this site is under an open source licence and is available here:"
    ),
    a(
      "A&E Tracker code repository",
      href = "https://github.com/HorridTom/AE-app"
    ),
    h2("Coming soon..."),
    p("In the near future we hope to implement the following new features:"),
    tags$ol(
      tags$li("..."),
      tags$li("...")
    )
  )
}
