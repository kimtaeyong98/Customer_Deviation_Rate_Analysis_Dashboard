  //마우스 아웃시 y축라인도 사라져버림 ㅜㅜ
  //760 ->1480  
  d3.json("./data./churn2.json",function(error,data){
    histogram1(data.churn,"All");
    histogram2(data.churn,"All");
    histogram3(data.churn,"All");
    histogram4(data.churn,"All");
  });
  function getMembership(){
    const membershipNodeList= document.getElementsByName('chk_info');
    d3.selectAll("#row3_2>*").remove();
    d3.selectAll("#row3_4>*").remove();
    d3.selectAll("#row3_1>*").remove();
    d3.selectAll("#row3_3>*").remove();
    membershipNodeList.forEach((node)=>{
      if(node.checked){
        d3.json("./data./churn2.json",function(error,data){
          histogram1(data.churn,node.value)
          histogram2(data.churn,node.value)
          histogram3(data.churn,node.value)
          histogram4(data.churn,node.value)
        });
      }
    })
  }
  function regioning(churndata,membershipgrade){
    var city=0
    var town=0
    var village=0
    for(let i=0;i<churndata.length;i++){
      if((membershipgrade==="All")&&(churndata[i].region_category=="City")){
        city++
      }
      else if((membershipgrade==="All")&&(churndata[i].region_category=="Town")){
        town++
      }
      else if((membershipgrade==="All")&&(churndata[i].region_category=="Village")){
        village++
      }
      if((churndata[i].membership_category===membershipgrade)&&(churndata[i].region_category=="City")){
        city++}
      else if((churndata[i].membership_category===membershipgrade)&&(churndata[i].region_category=="Town")){
        town++}
      else if((churndata[i].membership_category===membershipgrade)&&(churndata[i].region_category=="Village")){
        village++}
      else continue;
    }
    return [city, town, village];
  }
  function gendering(churndata,membershipgrade){
    var male=0
    var female=0
    for(let i=0;i<churndata.length;i++){
      if((membershipgrade==="All")&&(churndata[i].gender=="M")){
        male++
      }
      else if((membershipgrade==="All")&&(churndata[i].gender=="F")){
        female++
      }
      else if((churndata[i].membership_category===membershipgrade)&&(churndata[i].gender=="M")){
        male++}
      else if((churndata[i].membership_category===membershipgrade)&&(churndata[i].gender=="F")){
        female++}
      else continue;
    }
    return [male,female];
  }
  function aging(churndata,membershipgrade){
    var one =0
    var two=0
    var three=0
    var four =0
    var five=0
    var six=0
    var who=0
    for(let i=0;i<churndata.length;i++){
      var stringVal = churndata[i].age
      if((membershipgrade==="All")&&(stringVal>=60)){
        six++
      }
      else if((membershipgrade==="All")&&(stringVal>=50)){
        five++
      }
      else if((membershipgrade==="All")&&(stringVal>=40)){
        four++
      }
      else if((membershipgrade==="All")&&(stringVal>=30)){
        three++
      }
      else if((membershipgrade==="All")&&(stringVal>=20)){
        two++
      }
      else if((membershipgrade==="All")&&(stringVal>=10)){
        one++}
      else if((churndata[i].membership_category===membershipgrade)&&(stringVal>=60)){
        six++
      }
      else if((churndata[i].membership_category===membershipgrade)&&(stringVal>=50)){
        five++
      }
      else if((churndata[i].membership_category===membershipgrade)&&(stringVal>=40)){
        four++
      }
      else if((churndata[i].membership_category===membershipgrade)&&(stringVal>=30)){
        three++
      }
      else if((churndata[i].membership_category===membershipgrade)&&(stringVal>=20)){
        two++
      }
      else if((churndata[i].membership_category===membershipgrade)&&(stringVal>=10)){
        one++}  
      else continue;
    }
    return [one, two, three, four, five, six];
  }
  function yearing(churndata,membershipgrade){
    var five=0
    var six=0
    var seven=0
    for(let i=0;i<churndata.length;i++){
      var stringVal = churndata[i].join_date
      if((membershipgrade==="All")&&(stringVal.includes("2017"))){
        seven++}
      else if((membershipgrade==="All")&&(stringVal.includes("2016"))){
        six++}
      else if((membershipgrade==="All")&&(stringVal.includes("2015"))){
        five++}
      else if((churndata[i].membership_category===membershipgrade)&&(stringVal.includes("2017"))){
        seven++}
      else if((churndata[i].membership_category===membershipgrade)&&(stringVal.includes("2016"))){
        six++}
      else if((churndata[i].membership_category===membershipgrade)&&(stringVal.includes("2015"))){
        five++}
      else continue;
    }
    return [five,six,seven];
  }

  function histogram1(churndata,membershipgrade){//4_2번 차트입니다.
    var svgHeight=250;//svg높이
    var genders= gendering(churndata,membershipgrade);//gender데이터 가져옴
    var M=genders[0]//남자 수
    var F=genders[1]//여자 수
    var offsetX=110;
    var offsetY=10;
    var ay=0;
    var color_gender= ["#95caff","#ffb1e6"]
    if(membershipgrade=="All"){
      ay=17000;
    }
    else{
      ay=3500;
    }
    var div = d3.select("body").append("div")
                              .attr("class", "tooltip")
                              .style("opacity", 0);
    var yScale2 = d3.scaleLinear()
                        .domain([0,ay])
                        .range([svgHeight,0])
    var barElements = d3.select("#row3_2")
                        .append("svg")
                        .selectAll("rect")
                        //여기아님
                        .data(genders)
    //사각형
    barElements.enter()
              .append("rect")
              .attr("fill",function(d,i){return color_gender[i];})
              
              .attr("height",function(d,i){
                return d*svgHeight/ay;//도메인 크기만큼 줄임.
              })
              .attr("width",60)
              .attr("x",function(d,i){
                return i*65+offsetX;
              })
              .attr("y",function(d,i){
                return yScale2(d)-offsetY;
              })
              //마우스 오버
              .on("mouseover",function(d,i){
                d3.select(this)
                .style("fill","red")
                div.transition()
                  .duration(200)
                  .style("opacity",.9);
                div.html(d)
                  .style("left",(i*65+905+"px"))
                  .style("top",yScale2(d)+1495+"px")//760

                  barElements.enter().append('line')
                  .transition()
                  .duration(300)
                  .attr('x1', offsetX)
                  .attr('y1', yScale2(d)-offsetY)
                  .attr('x2', offsetX+160)
                  .attr('y2', yScale2(d)-offsetY)
                  .attr('stroke', 'red')
                  .attr('stroke-dasharray','4')
                })
                //마우스 아웃
              .on("mouseout",function(d,i){
                d3.select(this)
                  .style("fill",color_gender[i])
                div.transition()
                  .duration(500)
                  .style("opacity",0)
                d3.selectAll("line").style("display", "none")
              })
             

    //옆에 선
    d3.select("#row3_2").append("g")
                            .attr("class","axis")
                            .attr("transform","translate("+offsetX+","+(-offsetY)+")")
                            .attr("class","bary")
                            .call(
                              d3.axisLeft(yScale2)
                              .ticks(5)
                            )
    //밑에 선
    d3.select("#row3_2").append("rect")
                            .attr("class","axis_x")
                            .attr("width","140")
                            .attr("height","1")
                            .attr("transform","translate("+offsetX+","+(svgHeight-offsetY)+")")
    //남자 여자 텍스트
    barElements.enter()
              .append("text")
              .attr("class","barName")
              .attr("x",function(d,i){
                return i*65+30+offsetX;
              })
              .attr("y",svgHeight-offsetY+15)
              .text(function(d,i){
                return["남자","여자"][i];
              })
    barElements.enter()
              .append("text")
              .attr("class","linex")
              .attr("x",155)
              .attr("y",svgHeight-offsetY+45)
              .text("성별") 
    barElements.enter()
              .append("text")
              .attr("class","linex")
              .attr("x",40)
              .attr("y",-offsetY+35)
              .text("(명)")     
                  
  }


  function histogram2(churndata,membershipgrade){//4_4번 차트입니다.
    var svgHeight=250;//svg높이
    var years= yearing(churndata,membershipgrade);//year데이터 가져옴
    var fiv=years[0]//2015
    var six=years[1]//2016
    var seven=years[2]//2017
    var ay2=0;
    var color_year=["#955251","#92A8D1","#FF6F61"]
    if(membershipgrade=="All"){
      ay2=12000;
    }
    else{
      ay2=3000;
    }
    var div = d3.select("body").append("div")
                              .attr("class", "tooltip")
                              .style("opacity", 0);
    var offsetX=80;
    var offsetY=10;
    var yScale2 = d3.scaleLinear()
                        .domain([0,ay2])
                        .range([svgHeight,0])
    var barElements = d3.select("#row3_4")
                        .append("svg")
                        .selectAll("rect")
                        .data(years)
    barElements.enter()//사각형
              .append("rect")
              .attr("fill",function(d,i){return color_year[i];})
              .attr("height",function(d,i){
                return d*svgHeight/ay2;//도메인 크기만큼 줄임.
              })
              .attr("width",60)
              .attr("x",function(d,i){
                return i*65+offsetX;
              })
              .attr("y",function(d,i){
                return yScale2(d)-offsetY;
              })
              //마우스 오버
              .on("mouseover",function(d,i){
                d3.select(this)
                .style("fill","red")
                div.transition()
                  .duration(200)
                  .style("opacity",.9);
                div.html(d)
                  .style("left",(i*65+1685+"px"))
                  .style("top",yScale2(d)+1495+"px")

                  barElements.enter().append('line')
                  .transition()
                  .duration(300)
                  .attr('x1', offsetX)
                  .attr('y1', yScale2(d)-offsetY)
                  .attr('x2', offsetX+230)
                  .attr('y2', yScale2(d)-offsetY)
                  .attr('stroke', 'red')
                  .attr('stroke-dasharray','4')
                })
                //마우스 아웃
              .on("mouseout",function(d,i){
                d3.select(this)
                .style("fill",color_year[i])
                div.transition()
                  .duration(500)
                  .style("opacity",0)
                d3.selectAll("line").style("display", "none")
              })
              

    //옆에 선
    d3.select("#row3_4").append("g")
                            .attr("class","axis")
                            .attr("transform","translate("+offsetX+","+(-offsetY)+")")
                            .attr("class","bary")
                            .call(
                              d3.axisLeft(yScale2)
                              .ticks(5)
                            )
    //밑에 선
    d3.select("#row3_4").append("rect")
                            .attr("class","axis_x")
                            .attr("width","200")
                            .attr("height","1")
                            .attr("transform","translate("+offsetX+","+(svgHeight-offsetY)+")")
    //남자 여자 텍스트
    barElements.enter()
              .append("text")
              .attr("class","barName")
              .attr("x",function(d,i){
                return i*65+30+offsetX;
              })
              .attr("y",svgHeight-offsetY+15)
              .text(function(d,i){
                return["2015","2016","2017"][i];
              })
    //x축
    barElements.enter()
              .append("text")
              .attr("class","linex")
              .attr("x",145)
              .attr("y",svgHeight-offsetY+45)
              .text("가입년도")                  
    barElements.enter()
              .append("text")
              .attr("class","linex")
              .attr("x",10)
              .attr("y",-offsetY+45)
              .text("(명)")                       
  }
  function histogram3(churndata,membershipgrade){
    
    var svgHeight=250;//svg높이
    var regions= regioning(churndata,membershipgrade);//gender데이터 가져옴
    var city=regions[0]//남자 수
    var town=regions[1]//여자 수
    var village = regions[2]
    var offsetX=80;
    var offsetY=10;
    var ay3=0;
    var color_region=["#797979","#39B0A8","#ffdfcd"]
    console.log(membershipgrade)
    if(membershipgrade=="All"){
      ay3=17000;
    }
    else{
      ay3=3000;
    }
    var div = d3.select("body").append("div")
                              .attr("class", "tooltip")
                              .style("opacity", 0);
    var yScale2 = d3.scaleLinear()
                        .domain([0,ay3])
                        .range([svgHeight,0])
    var barElements = d3.select("#row3_1")
                        .selectAll("rect")
                        .data(regions)
    barElements.enter()
              .append("rect")
              .attr("fill",function(d,i){return color_region[i];})
              .attr("height",function(d,i){
                return d*svgHeight/ay3;//도메인 크기만큼 줄임.
              })
              .attr("width",60)
              .attr("x",function(d,i){
                return i*65+offsetX;
              })
              .attr("y",function(d,i){
                return yScale2(d)-offsetY;
              })
              .on("mouseover",function(d,i){
                d3.select(this)
                .style("fill","red")
                div.transition()
                  .duration(200)
                  .style("opacity",.9);
                div.html(d)
                  .style("left",(i*65+470+"px"))
                  .style("top",yScale2(d)+1495+"px")

                  barElements.enter().append('line')
                  .transition()
                  .duration(300)
                  .attr('x1', offsetX)
                  .attr('y1', yScale2(d)-offsetY)
                  .attr('x2', offsetX+230)
                  .attr('y2', yScale2(d)-offsetY)
                  .attr('stroke', 'red')
                  .attr('stroke-dasharray','4')
                })
                //마우스 아웃
              .on("mouseout",function(d,i){
                d3.select(this)
                 .style("fill",color_region[i])
                div.transition()
                  .duration(500)
                  .style("opacity",0)
                d3.selectAll("line").style("display", "none")

              })
              

    //옆에 선
    d3.select("#row3_1").append("g")
                            .attr("class","axis")
                            .attr("class","bary")
                            .attr("transform","translate("+offsetX+","+(-offsetY)+")")
                            .call(
                              d3.axisLeft(yScale2)
                              .ticks(5)//y축간격
                            )
    //밑에 선
    d3.select("#row3_1").append("rect")
                            .attr("class","axis_x")
                            .attr("width","220")
                            .attr("height","1")
                            .attr("transform","translate("+offsetX+","+(svgHeight-offsetY)+")")
    //남자 여자 텍스트
    barElements.enter()
              .append("text")
              .attr("class","barName")
              .attr("x",function(d,i){
                return i*65+30+offsetX;
              })
              .attr("y",svgHeight-offsetY+15)
              .text(function(d,i){
                return["city", "town","village"][i];
              })
    barElements.enter()
              .append("text")
              .attr("class","linex")
              .attr("x",145)
              .attr("y",svgHeight-offsetY+45)
              .text("주거지역") 
    barElements.enter()
              .append("text")
              .attr("class","linex")
              .attr("x",10)
              .attr("y",-offsetY+30)
              .text("(명)")                   
  }


  function histogram4(churndata,membershipgrade){
    var svgHeight=250;//svg높이
    var ages= aging(churndata,membershipgrade);//year데이터 가져옴
    var one=ages[0]//2015
    var two=ages[1]//2016
    var three=ages[2]//2017
    var four=ages[3]
    var five=ages[4]
    var six=ages[5]
    var offsetX=50;
    var offsetY=10;
    var ay4=0;
    var mouseleave = function(d) {
      div.transition()
        .duration(500)
        .style("opacity", 0)
      d3.select(this)
      .style("fill",function(d,i){return color_age[d];})
    }
    //안정해져서 임의로 넣은거에요 !!!!!!!!!!!!111111
    // var color_age=["#ff866d","#ffef76","#57ffcb","#ff7be2","#ffbc2d","#abff24"] 좀 수정했어요!!!!!!!!!!!
    var color_age=["#ff866d","#ff866d","#ffef76","#ffef76","#57ffcb","#57ffcb"]
    if(membershipgrade=="All"){
      ay4=6000;
    }
    else{
      ay4=2000;
    }
    var div = d3.select("body").append("div")
                              .attr("class", "tooltip")
                              .style("opacity", 0);
    var yScale2 = d3.scaleLinear()
                        .domain([0,ay4])
                        .range([svgHeight,0])
    var barElements = d3.select("#row3_3")
                        .selectAll("rect")
                        .data(ages)
    barElements.enter()
              .append("rect")
              .attr("fill",function(d,i){return color_age[i];})
              .attr("height",function(d,i){
                return d*svgHeight/ay4;//도메인 크기만큼 줄임.
              })
              .attr("width",40)
              .attr("x",function(d,i){
                return i*45+offsetX;
              })
              .attr("y",function(d,i){
                return yScale2(d)-offsetY;
              })
              .on("mouseover",function(d,i){
                d3.select(this)
                .style("fill","red")
                div.transition()
                  .duration(200)
                  .style("opacity",.9);
                div.html(d)
                  .style("left",(i*45+1240+"px"))
                  .style("top",yScale2(d)+1495+"px")

                  barElements.enter().append('line')
                  .transition()
                  .duration(300)
                  .attr('x1', 50)
                  .attr('y1', yScale2(d)-offsetY)
                  .attr('x2', 350)
                  .attr('y2', yScale2(d)-offsetY)
                  .attr('stroke', 'red')
                  .attr('stroke-dasharray','4')
                })
              .on("mouseout",function(d,i){
                  d3.select(this)
                  .style("fill",color_age[i])
                  div.transition()
                    .duration(500)
                    .style("opacity",0)
                  d3.selectAll("line").style("display", "none")
                })
            

    //옆에 선
    d3.select("#row3_3").append("g")
                            .attr("class","axis")
                            .attr("class","bary")
                            .attr("transform","translate("+offsetX+","+(-offsetY)+")")
                            .call(
                              d3.axisLeft(yScale2)
                              .ticks(5)
                            )
    //밑에 선
    d3.select("#row3_3").append("rect")
                            .attr("class","axis_x")
                            .attr("width","280")
                            .attr("height","1")
                            .attr("transform","translate("+offsetX+","+(svgHeight-offsetY)+")")
    //남자 여자 텍스트
    barElements.enter()
              .append("text")
              .attr("class","barName")
              .attr("x",function(d,i){
                return i*45+20+offsetX;
              })
              .attr("y",svgHeight-offsetY+15)
              .text(function(d,i){
                return["10대","20대","30대","40대","50대","60대"][i];
              })
    barElements.enter()
              .append("text")
              .attr("class","linex")
              .attr("x",10)
              .attr("y",-offsetY+30)
              .text("(명)")
    barElements.enter()
              .append("text")
              .attr("class","linex")
              .attr("x",155)
              .attr("y",svgHeight-offsetY+45)
              .text("연령")                           
  }