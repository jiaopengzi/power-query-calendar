let 
fxCalendar = (YearStart as number, YearEnd as number, FirstDayOfWeek as number, IsPrefix as text  ) as table =>
let
  TipText = 
            if not (FirstDayOfWeek >= 0 and FirstDayOfWeek < 7) then #table({"error"},{{"FirstDayOfWeek为 " & Text.From(FirstDayOfWeek) & ", 请输入正确数字 0 到 6 ."}}) 
            else if YearStart > YearEnd then #table({"error"},{{"YearStart为 " & Text.From(YearStart) & " 大于 YearEnd为 " & Text.From(YearEnd)}})
            else if not List.Contains({"YES","NO"}, IsPrefix) then #table({"error"},{{"IsPrefix为 " & Text.From(IsPrefix) & " 请输入正确内容: YES | NO." }})
            else "输入正确",
  yearlist = 
    let
      YearStart0 = YearStart, //开始年     
      YearEnd0   = YearEnd,   //结束年     
      yearlist0  = {YearStart0, YearEnd0}
    in
      List.Buffer(yearlist0), 

//常规日期表 type table 
  calendar1 = 
    let
      date_start  = #date(yearlist{0}, 1, 1),  //开始日期      
      date_end    = #date(yearlist{1}, 12, 31),  //结束日期      
      count       = Duration.Days(date_end - date_start),  //间隔天数      
      calendar0   = #table(
        type table
        [
          Dates           = Date.Type,                                                                                 //01、日期
          Day             = Int64.Type,                                                                                //02、日期：天
          WeekDay         = Int64.Type,                                                                                //04、周几数字
          WeekCNS1        = Text.Type,                                                                                 //04、周几中文简写1字
          WeekCNS3        = Text.Type,                                                                                 //05、周几中文简写3字
          WeekENS1        = Text.Type,                                                                                 //06、周几英文简写1字
          WeekENS3        = Text.Type,                                                                                 //07、周几英文简写3字
          WeekEN          = Text.Type,                                                                                 //08、周几英文
          WeekIndex       = Int64.Type,                                                                                //09、周索引：1900-1-1，星期一，第1周
          WeekNumber      = Int64.Type,                                                                                //10、周一开始的一年中第几周
          Week            = Text.Type,                                                                                 //11、周的W简写
          YearWeek        = Text.Type,                                                                                 //12、年周组合
          Month           = Int64.Type,                                                                                //13、月份数字
          MonthM          = Text.Type,                                                                                 //14、月份M简写
          MonthCN         = Text.Type,                                                                                 //15、月份中文
          MonthENS3       = Text.Type,                                                                                 //16、月份英文简写3字
          MonthEN         = Text.Type,                                                                                 //17、月份英文
          YearMonthM      = Text.Type,                                                                                 //18、年月M简写
          YearMonthUS     = Text.Type,                                                                                 //19、年月英文简写
          YearMonth       = Int64.Type,                                                                                //20、年月数字组合
          Quarter         = Int64.Type,                                                                                //21、季度数字
          QuarterQ        = Text.Type,                                                                                 //22、季度Q简写
          YearQuarterQ    = Text.Type,                                                                                 //23、年季度Q简写
          YearQuarter     = Int64.Type,                                                                                //24、年季度数字组合
          HalfOfYearCN    = Text.Type,                                                                                 //25、中文半年度
          HalfOfYearEN    = Text.Type,                                                                                 //26、半年度H简写
          YearHalf        = Text.Type,                                                                                 //27、年度季度简写组合
          Year            = Int64.Type,                                                                                //28、年度数字
          FY00            = Text.Type,                                                                                 //29、年度FY简写
          FY              = Text.Type,                                                                                 //30、FY：年、季度、月度等同维度展示辅助
          StartOfWeek     = Date.Type,                                                                                 //31、日期对应周开始的日期
          StartOfMonth    = Date.Type,                                                                                 //32、日期对应月开始的日期
          StartOfQuarter  = Date.Type,                                                                                 //33、日期对应季度开始的日期
          StartOfHalfYear = Date.Type,                                                                                 //34、日期对应半年度开始的日期
          StartOfYear     = Date.Type,                                                                                 //35、日期对应年度开始的日期
          EndOfWeek       = Date.Type,                                                                                 //36、日期对应周结束的日期
          EndOfMonth      = Date.Type,                                                                                 //37、日期对应月结束的日期
          EndOfQuarter    = Date.Type,                                                                                 //38、日期对应季度结束的日期
          EndOfHalfYear   = Date.Type,                                                                                 //39、日期对应半年度结束的日期
          EndOfYear       = Date.Type,                                                                                 //40、日期对应年度结束的日期
          DayOfYear       = Int64.Type                                                                                 //41、年中天数，1 月 1 日对应 1, 1 月 2 日对应 2, 以此类推，平年 12 月 31 日为 365，闰年为 366。
        ], 
        List.Transform(
          {0 .. count}, 
          (n) =>
            let
              d = Date.AddDays(date_start, n)
            in
              {
                d,                                                                                                      //01
                Date.Day(d),                                                                                            //02
                Date.DayOfWeek(d, 1),                                                                                   //03
                Text.End(Date.DayOfWeekName(d, "zh-CN"), 1),                                                            //04
                Date.DayOfWeekName(d, "zh-CN"),                                                                         //05
                Text.Start(Date.DayOfWeekName(d, "en-US"),1),                                                           //06
                Text.Start(Date.DayOfWeekName(d, "en-US"),3),                                                           //07
                Date.DayOfWeekName(d, "en-US"),                                                                         //08
                Number.RoundUp((Number.From(d) - 1) / 7, 0),                                                            //09
                Date.WeekOfYear(d, 1),                                                                                  //10
                "W" & Number.ToText(Date.WeekOfYear(d, 1), "00"),                                                       //11
                Date.ToText(d, "Yyy") & "W" & Number.ToText(Date.WeekOfYear(d, 1), "00"),                               //12
                Date.Month(d),                                                                                          //13
                "M" & Date.ToText(d, "MM"),                                                                             //14
                Date.MonthName(d, "zh-CN"),                                                                             //15
                Text.Start(Date.MonthName(d, "en-US"),3),                                                               //16
                Date.MonthName(d, "en-US"),                                                                             //17
                Date.ToText(d, "Yyy") & "M" & Date.ToText(d, "MM"),                                                     //18
                Text.Start(Date.MonthName(d, "en-US"),3) & "-" & Date.ToText(d, "yy"),                                  //19
                Date.Year(d) * 100 + Date.Month(d),                                                                     //20
                Date.QuarterOfYear(d),                                                                                  //21
                "Q" & Number.ToText(Date.QuarterOfYear(d)),                                                             //22
                Date.ToText(d, "Yyy") & "Q" & Number.ToText(Date.QuarterOfYear(d)),                                     //23
                Date.Year(d) * 100 + Date.QuarterOfYear(d),                                                             //24
                if Date.Month(d) < 7 then "上半年" else "下半年",                                                        //25
                if Date.Month(d) < 7 then "H1" else "H2",                                                               //26
                if Date.Month(d) < 7 then Date.ToText(d, "Yyy") & "H1" else Date.ToText(d, "Yyy") & "H2",               //27
                Date.Year(d),                                                                                           //28
                "FY" & Date.ToText(d, "yy"),                                                                            //29
                "FY",                                                                                                   //30
                Date.StartOfWeek(d),                                                                                    //31
                Date.StartOfMonth(d),                                                                                   //32
                Date.StartOfQuarter(d),                                                                                 //33
                if Date.Month(d) < 7 then Date.StartOfYear(d) else Date.AddMonths(Date.StartOfYear(d), 6),              //34
                Date.StartOfYear(d),                                                                                    //35
                Date.EndOfWeek(d),                                                                                      //36
                Date.EndOfMonth(d),                                                                                     //37
                Date.EndOfQuarter(d),                                                                                   //38
                if Date.Month(d) > 6 then Date.EndOfYear(d) else Date.AddMonths(Date.EndOfYear(d), - 6),                //39
                Date.EndOfYear(d),                                                                                      //40
                Date.DayOfYear(d)                                                                                       //41
             }
       )
     )
    in
      Table.Buffer(calendar0),
      
/*所有列名增加编号前缀*/
    calendarAllNumber =  
        let NewNames = List.Transform( Table.ColumnNames( calendar1 ) ,(name) => {name,"C"& Number.ToText(List.PositionOf(Table.ColumnNames( calendar1 ),name) + 1,"00_") & name} ),
            calendarAllNumber = Table.RenameColumns( calendar1, NewNames )
        in
        calendarAllNumber

in 
  if TipText = "输入正确" and IsPrefix = "YES" then calendarAllNumber
  else if TipText = "输入正确" and IsPrefix = "NO" then calendar1
  else TipText,


//自定义函数原数据
  _TypeMeta = type function (
    YearStart as (
      type number
        meta [
          Documentation.FieldCaption  = "1参：YearStart 日期表开始年份.", 
          Documentation.SampleValues  = {2020}
        ]
   ), 
    YearEnd as (
      type number
        meta [
          Documentation.FieldCaption  = "2参：YearEnd 日期表结束年份.", 
          Documentation.SampleValues  = {2022}
        ]
   ),
    FirstDayOfWeek as (
      type number
        meta [
          Documentation.FieldCaption  = "3参：FirstDayOfWeek 将周一至周日哪一天视为新一周的开始.", 
          Documentation.SampleValues  = {1},
          Documentation.AllowedValues = {0 .. 6}
        ]
   ),
    IsPrefix as (
      type text
        meta [
          Documentation.FieldCaption  = "4参: IsPrefix 字段名称是否增加数字前置.", 
          Documentation.SampleValues  = {"YES"},
          Documentation.AllowedValues = {"YES","NO"}
        ]
   )
 ) as table
    meta [
      Documentation.Name = "fxCalendarNoLunar", 
      Documentation.LongDescription
        = "返回一个日期表,日期表不包含农历日期信息.
        <ul>1参: <code>YearStart</code>  日期表开始年份 </ul>
        <ul>2参: <code>YearEnd</code>  日期表结束年份;</ul>
        <ul>3参: <code>FirstDayOfWeek</code>  将周一至周日哪一天视为一周的开始, 周日:0, 周一:1, 周二:2, 周三:3, 周四:4, 周五:5, 周六:6; </ul>
        <ul>4参: <code>IsPrefix</code>  字段名称是否增加数字前置.</ul>", 
      Documentation.WhoAskedTheRightQuestion = "www.jiaopengzi.com", 
      Documentation.Author = "焦棚子", 
      Documentation.Examples = {
        [
          Description = "返回包含农历日期信息的日期表.", 
          Code = "fxCalendar(2020, 2022, 1, ""YES"")", 
          Result = "返回 2020 年到 2022 年的不包含农历日期信息的日期表;周一为新一周的开始;字段名称增加数字前缀."
        ]
     }
    ]
in
  Value.ReplaceType(fxCalendar, _TypeMeta)