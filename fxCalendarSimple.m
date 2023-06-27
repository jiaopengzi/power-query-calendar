let 
fxCalendar = (YearStart as number, YearEnd as number, FirstDayOfWeek as number, IsPrefix as text ) as table =>
let
  TipText = 
            if not (FirstDayOfWeek >= 0 and FirstDayOfWeek < 7) then #table({"error"},{{"FirstDayOfWeek为 " & Text.From(FirstDayOfWeek) & ", 请输入正确数字 0 到 6 ."}}) 
            else if YearStart > YearEnd then #table({"error"},{{"YearStart为 " & Text.From(YearStart) & " 大于 YearEnd为 " & Text.From(YearEnd)}})
            else if not List.Contains({"YES","NO"}, IsPrefix) then #table({"error"},{{"IsPrefix为 " & Text.From(IsPrefix) & " 请输入正确内容: YES | NO." }})
            else "输入正确",
  yearlist = 
    let
      YearStart0 = YearStart, /*开始年*/     
      YearEnd0   = YearEnd,   /*结束年*/     
      yearlist0  = {YearStart0, YearEnd0}
    in
      List.Buffer(yearlist0), 
  lunarColumn = 
    let
      lunarColumn0 =
      [
        Name    = {"animal", "day", "lDate", "lMonth", "lunarDate", "lunarMonth", "lunarYear", "month", "year", "status"},                  /*爬取的名称list*/
        Type    = {Text.Type, Int64.Type, Text.Type, Text.Type, Int64.Type, Int64.Type, Int64.Type, Int64.Type, Int64.Type, Text.Type},  	/*爬取的名称list的类型,统一类型后再合并*/
        Select  = {"animal", "lDate", "lMonth", "lunarDate", "lunarMonth", "lunarYear", "status"},                                          /*爬取的农历信息最后需要的列*/
        New     = {"Animal", "LunarDateCN", "LunarMonthCN", "LunarDate", "LunarMonth", "LunarYear", "Status"}   /*对最终需要的列名称进行统一*/
      ]
    in
      lunarColumn0, 

/*常规日期表 type table */
  calendar1 = 
    let
      date_start  = #date(yearlist{0}, 1, 1),  /*开始日期*/    
      date_end    = #date(yearlist{1}, 12, 31),  /*结束日期*/      
      count       = Duration.Days(date_end - date_start),  /*间隔天数*/      
      calendar0   = #table(
        type table
        [
          Dates                 = Date.Type,                    /*01、日期*/
          Day                   = Int64.Type,                   /*02、日期:天*/
          WeekDay               = Int64.Type,                   /*03、周几数字*/
          WeekCNS1              = Text.Type,                    /*04、周几中文简写1字*/
          WeekCNS3              = Text.Type,                    /*05、周几中文简写3字*/
          WeekENS1              = Text.Type,                    /*06、周几英文简写1字*/
          WeekENS3              = Text.Type,                    /*07、周几英文简写3字*/
          WeekEN                = Text.Type,                    /*08、周几英文*/
          WeekIndex             = Int64.Type,                   /*09、周索引:1900-1-1,星期一,第1周*/
          WeekNumber            = Int64.Type,                   /*10、周一开始的一年中第几周*/
          Week                  = Text.Type,                    /*11、周的W简写*/
          YearWeek              = Text.Type,                    /*12、年周组合*/
          Month                 = Int64.Type,                   /*13、月份数字*/
          MonthM                = Text.Type,                    /*14、月份M简写*/
          MonthCN               = Text.Type,                    /*15、月份中文*/
          MonthENS3             = Text.Type,                    /*16、月份英文简写3字*/
          MonthEN               = Text.Type,                    /*17、月份英文*/
          YearMonthM            = Text.Type,                    /*18、年月M简写*/
          YearMonthUS           = Text.Type,                    /*19、年月英文简写*/
          YearMonth             = Int64.Type,                   /*20、年月数字组合*/
          Quarter               = Int64.Type,                   /*21、季度数字*/
          QuarterQ              = Text.Type,                    /*22、季度Q简写*/
          YearQuarterQ          = Text.Type,                    /*23、年季度Q简写*/
          YearQuarter           = Int64.Type,                   /*24、年季度数字组合*/
          HalfOfYearCN          = Text.Type,                    /*25、中文半年度*/
          HalfOfYearEN          = Text.Type,                    /*26、半年度H简写*/
          YearHalf              = Text.Type,                    /*27、年度季度简写组合*/
          Year                  = Int64.Type,                   /*28、年度数字*/
          FY00                  = Text.Type,                    /*29、年度FY简写*/
          FY                    = Text.Type,                    /*30、FY:年、季度、月度等同维度展示辅助*/
          StartOfWeek           = Date.Type,                    /*31、日期对应周开始的日期*/
          StartOfMonth          = Date.Type,                    /*32、日期对应月开始的日期*/
          StartOfQuarter        = Date.Type,                    /*33、日期对应季度开始的日期*/
          StartOfHalfYear       = Date.Type,                    /*34、日期对应半年度开始的日期*/
          StartOfYear           = Date.Type,                    /*35、日期对应年度开始的日期*/
          EndOfWeek             = Date.Type,                    /*36、日期对应周结束的日期*/
          EndOfMonth            = Date.Type,                    /*37、日期对应月结束的日期*/
          EndOfQuarter          = Date.Type,                    /*38、日期对应季度结束的日期*/
          EndOfHalfYear         = Date.Type,                    /*39、日期对应半年度结束的日期*/
          EndOfYear             = Date.Type,                    /*40、日期对应年度结束的日期*/
          DayOfWeek             = Int64.Type,                   /*41、日期对应周的当前累计天数*/
          DayOfMonth            = Int64.Type,                   /*42、日期对应月的当前累计天数*/
          DayOfQuarter          = Int64.Type,                   /*43、日期对应季度的当前累计天数*/
          DayOfHalfYear         = Int64.Type,                   /*44、日期对应半年度的当前累计天数*/
          DayOfYear             = Int64.Type,                   /*43、日期对应年度的当前累计天数,1 月 1 日对应 1, 1 月 2 日对应 2, 以此类推,平年 12 月 31 日为 365,闰年为 366.*/
          DaysOfWeek            = Int64.Type,                   /*46、日期对应周的总计天数*/
          DaysOfMonth           = Int64.Type,                   /*47、日期对应月的总计天数*/
          DaysOfQuarter         = Int64.Type,                   /*48、日期对应季度的总计天数*/
          DaysOfHalfYear        = Int64.Type,                   /*49、日期对应半年度的总计天数*/
          DaysOfYear            = Int64.Type,                   /*50、日期对应年度的总计天数*/
          ProgressOfWeek        = Percentage.Type,              /*51、日期对应周的日期进度百分比*/
          ProgressOfMonth       = Percentage.Type,              /*52、日期对应月的日期进度百分比*/
          ProgressOfQuarter     = Percentage.Type,              /*53、日期对应季度的日期进度百分比*/
          ProgressOfHalfYear    = Percentage.Type,              /*54、日期对应半年度的日期进度百分比*/
          ProgressOfYear        = Percentage.Type               /*55、日期对应年度的日期进度百分比*/
], 
        List.Transform(
          {0 .. count}, 
          (n) =>
            let
              d = Date.AddDays(date_start, n)
            in
              {
                /*01*/ d,
                /*02*/ Date.Day(d),
                /*03*/ Date.DayOfWeek(d, 1),
                /*04*/ Text.End(Date.DayOfWeekName(d, "zh-CN"), 1),
                /*05*/ Date.DayOfWeekName(d, "zh-CN"),
                /*06*/ Text.Start(Date.DayOfWeekName(d, "en-US"),1),
                /*07*/ Text.Start(Date.DayOfWeekName(d, "en-US"),3),
                /*08*/ Date.DayOfWeekName(d, "en-US"),
                /*09*/ Number.RoundUp((Number.From(d) - 1) / 7, 0),
                /*10*/ Date.WeekOfYear(d, FirstDayOfWeek),
                /*11*/ "W" & Number.ToText(Date.WeekOfYear(d, FirstDayOfWeek), "00"),
                /*12*/ Date.ToText(d, "Yyy") & "W" & Number.ToText(Date.WeekOfYear(d, FirstDayOfWeek), "00"),
                /*13*/ Date.Month(d),
                /*14*/ "M" & Date.ToText(d, "MM"),
                /*15*/ Date.MonthName(d, "zh-CN"),
                /*16*/ Text.Start(Date.MonthName(d, "en-US"),3),
                /*17*/ Date.MonthName(d, "en-US"),
                /*18*/ Date.ToText(d, "Yyy") & "M" & Date.ToText(d, "MM"),
                /*19*/ Text.Start(Date.MonthName(d, "en-US"),3) & "-" & Date.ToText(d, "yy"),
                /*20*/ Date.Year(d) * 100 + Date.Month(d),
                /*21*/ Date.QuarterOfYear(d),
                /*22*/ "Q" & Number.ToText(Date.QuarterOfYear(d)),
                /*23*/ Date.ToText(d, "Yyy") & "Q" & Number.ToText(Date.QuarterOfYear(d)),
                /*24*/ Date.Year(d) * 100 + Date.QuarterOfYear(d),
                /*25*/ if Date.Month(d) < 7 then "上半年" else "下半年",
                /*26*/ if Date.Month(d) < 7 then "H1" else "H2",
                /*27*/ if Date.Month(d) < 7 then Date.ToText(d, "Yyy") & "H1" else Date.ToText(d, "Yyy") & "H2",
                /*28*/ Date.Year(d),
                /*29*/ "FY" & Date.ToText(d, "yy"),
                /*30*/ "FY",
                /*31*/ Date.StartOfWeek(d),
                /*32*/ Date.StartOfMonth(d),
                /*33*/ Date.StartOfQuarter(d),
                /*34*/ if Date.Month(d) < 7 then Date.StartOfYear(d) else Date.AddMonths(Date.StartOfYear(d), 6),
                /*35*/ Date.StartOfYear(d),
                /*36*/ Date.EndOfWeek(d),
                /*37*/ Date.EndOfMonth(d),
                /*38*/ Date.EndOfQuarter(d),
                /*39*/ if Date.Month(d) > 6 then Date.EndOfYear(d) else Date.AddMonths(Date.EndOfYear(d), - 6),
                /*40*/ Date.EndOfYear(d),
                /*41*/ Date.DayOfWeek(d, 1)+1,
                /*42*/ Date.Day(d),
                /*43*/ Duration.Days(d - Date.StartOfQuarter(d)) + 1,
                /*44*/ Duration.Days(d - (if Date.Month(d) < 7 then Date.StartOfYear(d) else Date.AddMonths(Date.StartOfYear(d), 6))) + 1,
                /*45*/ Date.DayOfYear(d),
                /*46*/ 7,
                /*47*/ Date.Day(Date.EndOfMonth(d)),
                /*48*/ Duration.Days(Date.EndOfQuarter(d) - Date.StartOfQuarter(d))+1,

                /*49*/ Duration.Days((if Date.Month(d) > 6 then Date.EndOfYear(d) else Date.AddMonths(Date.EndOfYear(d), - 6)) - 
                        (if Date.Month(d) < 7 then Date.StartOfYear(d) else Date.AddMonths(Date.StartOfYear(d), 6))) + 1,

                /*50*/ Duration.Days(Date.EndOfYear(d)-Date.StartOfYear(d)) + 1,
                /*51*/ (Date.DayOfWeek(d, 1)+1)/7,
                /*52*/ Date.Day(d)/Date.Day(Date.EndOfMonth(d)),
                /*53*/ (Duration.Days(d - Date.StartOfQuarter(d)) + 1)/(Duration.Days(Date.EndOfQuarter(d) - Date.StartOfQuarter(d))+1),

                /*54*/ (Duration.Days(d - (if Date.Month(d) < 7 then Date.StartOfYear(d) else Date.AddMonths(Date.StartOfYear(d), 6))) + 1)/
                        (Duration.Days((if Date.Month(d) > 6 then Date.EndOfYear(d) else Date.AddMonths(Date.EndOfYear(d), - 6)) - 
                            (if Date.Month(d) < 7 then Date.StartOfYear(d) else Date.AddMonths(Date.StartOfYear(d), 6))) + 1),

                /*55*/ Date.DayOfYear(d)/(Duration.Days(Date.EndOfYear(d)-Date.StartOfYear(d))+1)
             }
       )
     )
    in
      Table.Buffer(calendar0), 

/*法定假期表格 1900-2050年.*/
  holidayTable = 
    let
      holiday_source  = Json.Document(
        /*
        Web.Contents("https://sp1.baidu.com/8aQDcjqpAAV3otqbppnN2DJv/api.php?&query=法定节假日&tn=wisetpl&format=json&resource_id=39042&oe=utf8")
        2023-6-27 将上述方法修改为如下参数模式,解决:云端无法 刷新 动态数据源 问题。
        参考：https://aka.ms/dynamic-data-sources
        参考：https://learn.microsoft.com/en-us/power-bi/connect-data/refresh-data#refresh-and-dynamic-data-sources
        */
        Web.Contents(
          "https://sp1.baidu.com",
          [
            RelativePath = "8aQDcjqpAAV3otqbppnN2DJv/api.php",
            Query = [query="法定节假日",tn="wisetpl",format="json",resource_id="39042",oe="utf8"]
          ]
        )
      ), 
      holidayList     = holiday_source[data]{0}[holiday], 
      holidayTable1   = Table.FromRecords(List.Combine(List.Transform(holidayList, each _[list]))), 
      holidayTable2   = Table.TransformColumnTypes(holidayTable1, {{"date", type date}, {"name", type text}}),
      holidayTable3   = Table.Group(holidayTable2, {"date"}, {"name", each Text.Combine([name] ,",") ,type text}) /*去重节日重复的情况,比如中秋国亲重叠*/
    in
      Table.Buffer(holidayTable3),

/*日期信息函数封装,接口只满足:2022年5月,按照月份的形式获取,获取的是该月份前中后三个月的信息;注意编码参数:oe=utf8,其它的对中文乱码*/
  lunarDateTableX = (year0 as number, month0 as number) =>
    let
      lunarDate_source  = Json.Document(
        /*
        Web.Contents("https://sp1.baidu.com/8aQDcjqpAAV3otqbppnN2DJv/api.php?&query=" & Text.From(year0) & "年" & Text.From(month0) & "月&tn=wisetpl&format=json&resource_id=39043&oe=utf8")
        2023-6-27 将上述方法修改为如下参数模式,解决:云端无法 刷新 动态数据源 问题。
        参考：https://aka.ms/dynamic-data-sources
        参考：https://learn.microsoft.com/en-us/power-bi/connect-data/refresh-data#refresh-and-dynamic-data-sources
        */
        Web.Contents(
            "https://sp1.baidu.com",
            [
              RelativePath = "8aQDcjqpAAV3otqbppnN2DJv/api.php",
              Query = [ query=Text.From(year0)& "年"& Text.From(month0)& "月", tn="wisetpl", format="json", resource_id="39043",oe="utf8"]
            ]
        )
      ), 
      almanac           = lunarDate_source[data]{0}[almanac], 
      lunarDateTable3   = Table.Combine(List.Transform(almanac, each Table.FromRecords({_}))), 
      lunarDateTable1   = Table.SelectRows(Table.SelectColumns(lunarDateTable3, lunarColumn[Name], MissingField.UseNull), each [month] = Text.From(month0))
    in
      lunarDateTable1, 

/*通过lunarDateTableX获取对应年份的农历日期信息*/
  lunarDateTableN = 
    let
      YearList            = List.Buffer({yearlist{0} .. yearlist{1}}), 
      MonthList           = List.Buffer({1 .. 12}), 
      lunarDateList       = List.Buffer(List.Transform(YearList, (y) => List.Transform(MonthList, (m) => lunarDateTableX(y, m)))), 
      lunarDateTable      = Table.Combine(List.Combine(lunarDateList)), 
      lunarDateTableType  = Table.TransformColumnTypes(lunarDateTable, List.Zip({lunarColumn[Name], lunarColumn[Type]}))
    in
      Table.Buffer(lunarDateTableType), 
      
/*常规日期、法定假期、农历信息 合并拼接*/
  calendarAll = 
    let
      t1 = Table.Buffer(Table.NestedJoin(calendar1, {"Dates"}, holidayTable, {"date"}, "holiday", JoinKind.LeftOuter)), 
      t2 = Table.Buffer(Table.ExpandTableColumn(t1, "holiday", {"name"}, {"Holiday"})), 
      t3 = Table.Buffer(Table.NestedJoin(t2, {"Year", "Month", "Day"}, lunarDateTableN, {"year", "month", "day"}, "lunarDate", JoinKind.LeftOuter)), 
      t4 = Table.Buffer(Table.ExpandTableColumn(t3, "lunarDate", lunarColumn[Select], lunarColumn[New])),
      t5 = Table.Buffer(Table.ReplaceValue(t4,each _,null,(X,Y,Z) => if Y[Status] = "1" then "假期" else if Y[Status] = "2" then "补班" else if Y[Status] is null  and Y[WeekDay] < 5  then "工作日"  else "周末" , {"Status"})),
      t6 = Table.Buffer(Table.Sort(Table.TransformColumnTypes(t5, {"Status", type text}), {"Dates", Order.Ascending}))
    in
      t6,
    
/*所有列名增加编号前缀*/
    calendarAllNumber =  
        let NewNames = List.Transform( Table.ColumnNames( calendarAll ) ,(name) => {name,"C"& Number.ToText(List.PositionOf(Table.ColumnNames( calendarAll ),name) + 1,"00_") & name} ),
            calendarAllNumber = Table.RenameColumns( calendarAll, NewNames )
        in
        calendarAllNumber

in 
  if TipText = "输入正确" and IsPrefix = "YES" then calendarAllNumber
  else if TipText = "输入正确" and IsPrefix = "NO" then calendarAll
  else TipText
  ,

/*自定义函数元数据*/
  _TypeMeta = type function (
    YearStart as (
      type number
        meta [
          Documentation.FieldCaption  = "1参: YearStart 日期表开始年份.", 
          Documentation.SampleValues  = {2020}
        ]
   ), 
    YearEnd as (
      type number
        meta [
          Documentation.FieldCaption  = "2参: YearEnd 日期表结束年份.", 
          Documentation.SampleValues  = {2022}
        ]
   ),
    FirstDayOfWeek as (
      type number
        meta [
          Documentation.FieldCaption  = "3参: FirstDayOfWeek 将周一至周日哪一天视为新一周的开始.", 
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
      Documentation.Name = "fxCalendar", 
      Documentation.LongDescription
        = "返回一个日期表,日期表包含农历日期信息.<ul>1参: <code>YearStart</code>  日期表开始年份 </ul> <ul>2参: <code>YearEnd</code>  日期表结束年份;</ul> <ul>3参: <code>FirstDayOfWeek</code>  将周一至周日哪一天视为一周的开始, 周日:0, 周一:1, 周二:2, 周三:3, 周四:4, 周五:5, 周六:6; </ul><ul>4参: <code>IsPrefix</code>  字段名称是否增加数字前置.</ul>", 
      Documentation.WhoAskedTheRightQuestion = "www.jiaopengzi.com", 
      Documentation.Author = "焦棚子", 
      Documentation.Examples = {
        [
          Description = "返回包含农历日期信息的日期表.", 
          Code = "fxCalendar(2020, 2022, 1, ""YES"")", 
          Result = "返回 2020 年到 2022 年的包含农历日期信息的日期表;周一为新一周的开始;字段名称增加数字前缀."
        ]
     }
    ]
in
  Value.ReplaceType(fxCalendar, _TypeMeta)