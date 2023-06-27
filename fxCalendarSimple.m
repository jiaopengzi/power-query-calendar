//简单版日期表 
let
      date_start  = #date(2022, 1, 1),  //开始日期      
      date_end    = #date(2022, 12, 31),  //结束日期      
      count       = Duration.Days(date_end - date_start),  //间隔天数      
      calendar0   = #table(
        type table
        [
          日期            = Date.Type,                                                                                 //01、日期
          天              = Int64.Type,                                                                                //02、日期：天
          月份            = Text.Type,                                                                                 //14、月份M简写
          年月            = Text.Type,                                                                                 //18、年月M简写
          年              = Text.Type                                                                                  //29、年度FY简写

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
                "M" & Date.ToText(d, "MM"),                                                                             //14
                Date.ToText(d, "Yyy") & "M" & Date.ToText(d, "MM"),                                                     //18
                "FY" & Date.ToText(d, "yy")                                                                             //29

             }
       )
     )
in
      calendar0