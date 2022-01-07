
  //전체 데이터
  var jsonRepository = {};
  
  //열
  var churn_score_List=[];
  var joining_date_List=[];
  var feedback_List=[];
  
//깃에서 데이터가져오기
  $.ajax({
    url: "https://raw.githubusercontent.com/jungsu309/2021teampj/master/data/final_data.json",
    dataType: "json"
  }).done(function (result) {
    jsonRepository = result;
    splitData()
  });
  
  
  function splitData() {
    for (var i = 0; i < jsonRepository.length; i++) {
      // 출력하고싶은 열들 리스트 만든거에 넣어주기.. (전부 다 넣는다.)
      churn_score_List.push(jsonRepository[i]['churn_risk_score']);
	  joining_date_List.push(jsonRepository[i]['join_date']);
	  feedback_List.push(jsonRepository[i]['feedback']);
      
    }
	//확인됨
	//console.log(churn_score_List);
	//console.log(joining_date_List);
	//console.log(feedback_List);
	
	//이탈도 빈도(막대그래프)
	//빈도 세고싶은 컬럼, 가로길이, 세로길이,
	type_count(churn_score_List,400,300);
	
	//피드백 빈도(막대그래프)
	type_count2(feedback_List,800,300);
	
	//날짜별 이탈 위험도.(이탈 위험도 계산 필요)
	//날짜랑 위험도랑 짝짓기
	match_risk(joining_date_List,churn_score_List);
  }
	
  function type_count(data,w,h){
	//빈도를 세서 담아줄 곳
	const dict = {};
	
	data.forEach((x) => { 
	  dict[x] = (dict[x] || 0)+1; 
    });
	//console.log(d3.max(dict));
	
	//key값
	var key = Object.keys(dict);
	
	//value값을 담아주기
	var value=[];
	key.forEach(function(key){
      value.push(dict[key]);
	});
	
	var max = Math.ceil(d3.max(value)/1000)*1000;
	//console.log(dict);
	//console.log(key);//키값만 담아둠 ->x축에 넣어줄거임
	//console.log(value);//빈도만 담아둠
	
	//그래프 그리기
    draw_barplot(key,value,w,h,max);// 키값과 빈도를 넘김
  }

  function type_count2(data,w,h){
	//빈도를 세서 담아줄 곳
	const dict = {};
	
	data.forEach((x) => { 
	  dict[x] = (dict[x] || 0)+1; 
    });
	//console.log(dict);

	var keyValues = [];
	for (var key in dict) {
	  keyValues.push([ key, dict[key] ])
	}
	
	//내림차순 정렬
	keyValues.sort(function compare(kv1, kv2) {
	  return kv2[1] - kv1[1]
	})
	
	var key=[];
	var value=[];
	for(var i = 0; i < keyValues.length; i++){
	  key.push(keyValues[i][0]);
	  value.push(keyValues[i][1]);
	}
	
	var max = Math.ceil(d3.max(value)/1000)*1000;
	//console.log(dict);
	//console.log(keyValues);
	//console.log(key);//키값만 담아둠 ->x축에 넣어줄거임
	//console.log(value);//빈도만 담아둠
	
	//그래프 그리기
    draw_barplot2(key,value,w,h,max);// 키값과 빈도를 넘김
  }

  
  function match_risk(date,riskdata){
    var match_dict=[];
	//console.log(date);
	for(var i =0; i<date.length; i++){
	  match_dict.push({date : date[i], churn_score : riskdata[i]})
	}
	//console.log("날짜와 위험도 매칭");
	//console.log(match_dict);
	  
	
	//이탈위험도 평균을 계산해서 list에 넣기. brush나 filter로 기간 지정해줘야함.
	//일단은 1년 단위로 해보기로했다. 예 -2015년(범위..?)
	var churn_mean_list=[];//y축(값)
	var date_list=["01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"];//x축
	
	//
	for (var i = 0; i < date_list.length; i++){//date_list
	  var churn_list=[];
	  
	  for (var j = 0; j <match_dict.length; j++){//match_dict
	    if(match_dict[j]['date'].split('-')[0]== '2017' && match_dict[j]['date'].split('-')[1]== date_list[i]){
		//해당 기간의 churn만 모음
		  churn_list.push(match_dict[j]['churn_score']);
		}
	  }
	
	  var churn_sum = 0;
	  for (var n = 0; n < churn_list.length; n++){
	    churn_sum = churn_sum + churn_list[n];
	  }
	  churn_mean = churn_sum/churn_list.length;
	  churn_mean_list.push(churn_mean);
	}
	//console.log(date_list);//x축
	//console.log(churn_mean_list);//y축(값)->반올림 시키는것이 더 깔끔해보임.

	//draw_barplot(date_list, churn_mean_list, 300,300,100,100,5);
	//draw_lineplot(date_list, churn_mean_list, 300,300,50,400,5);
  }



  //x축데이터랑 값을 파라미터로 전달
  //위치는 나중에 배치 시켜야될 것 같다 
  function draw_barplot(xdata, dict_data, w, h, max){
	//그래프 전체적인 크기 설정하고, body에 생성해주는 식
    var width = w, height = h;
    var offsetX=100;
    var offsetY=30;
	var color = ["rgb(31,119,180)","rgb(255,127,14)","rgb(44,160,44)"];
   
    //html 상의 위치 정해주기
    var svg = d3.select("#row2_1").append("svg")
      .attr("width", width)
      .attr("height", height)
	
	var yScale = d3.scaleLinear().domain([0, max])//축 범위  d3.max(dict_data)
	  .range([height, 2.5*offsetY]);//축 너비?? 길이? 조정
		 
	var yAxis = d3.axisLeft().scale(yScale);
	 
	svg.append("g")
	  .attr("transform","translate("+offsetX+","+(-offsetY)+")")
	  .attr("class", "bary")
	  .call(yAxis)
	  .style("font-size", "15px");
	
	var div1 = d3.select("body").append("div")
      .attr("class", "tooltip")
      .style("opacity", 0);
	
	// Three function that change the tooltip when user hover / move / leave a cell
	var mouseover = function(d) {
	  div1.style("opacity", 1)
	  d3.select(this)
		.style("fill","red")
	}
	var mousemove = function(d) {
	  div1.html(d)
        .style("left", (d3.event.pageX)-30 + "px")
        .style("top", (d3.event.pageY -40) + "px");
	  //console.log(d);
	  div1.transition()
        .duration(200)
        .style("opacity", .9);
	}
	var mouseleave = function(d) {
	  d3.select(this)
		.style("fill",function(d,i){return color[d];});
	  div1.transition()
        .duration(500)
        .style("opacity", 0);
	}
		 
	//사각형을 그리는 rect를 생성해서 svg에 추가하기
	svg.selectAll("rect")
	  .data(dict_data)
	  .enter()
	  .append("rect")
	  .attr("x", function(d, i){
		return  i*((width-offsetX)/(dict_data.length)) + 0.9*(width)/(dict_data.length) ;// svg 크기에 맞게 각각의 막대 자리를 나눠줌
	  })
	  .attr("y", function(d,i){
		return yScale(d)-offsetY;
	  })
	  .attr("width", 60)//굵기
	  .attr("height",function(d,i){return d*(height-2.5*offsetY)/max;})
	  .attr("fill",function(d,i){return color[i];})
	  .on("mouseover", mouseover)
	  .on("mousemove", mousemove)
	  .on("mouseleave", mouseleave);
	  
	//X axis: scale and draw:
	var x = d3.scaleBand()
	  .domain(xdata)//key값을 x축으로
	  .range([0, width-offsetX]);//축 길이?? 사이즈
	    
	svg.append("g")
	  .attr("transform","translate("+offsetX+","+(height-offsetY)+")")
	  .attr("class", "barName")
	  .call(d3.axisBottom(x))
	  .style("font-size","15px");

  }


  function draw_barplot2(xdata, dict_data, w, h, max){
	//그래프 전체적인 크기 설정하고, body에 생성해주는 식
    var width = w, height = h;
    var offsetX=220;
    var offsetY=30;
	
	var color = ["rgb(212,122,254)","rgb(212,122,254)","rgb(212,122,254)","rgb(212,122,254)","rgb(212,122,254)",
				"rgb(172,255,110)","rgb(172,255,110)","rgb(172,255,110)","rgb(172,255,110)"];
   
    //html 상의 위치 정해주기
    var svg = d3.select("#row2_2").append("svg")
	  .attr("width", width)
	  .attr("height", height)
		 
	var xScale = d3.scaleLinear().domain([0, max])//축 범위
	  .range([0,width-(offsetX+110)]);//축 너비?? 길이? 조정
		 
	var xAxis = d3.axisBottom().scale(xScale);
	 
	svg.append("g")
	  .attr("transform","translate("+offsetX+","+(height-offsetY)+")")
	  .attr("class", "barName")
	  .call(xAxis)
	  .style("font-size", "12px");
	
	var div1 = d3.select("body")
	  .append("div")
	  .attr("class", "tooltip")
	  .style("opacity", 0);
	
	var mouseover = function(d) {
	  div1.style("opacity", 1);
	  d3.select(this)
		.style("fill","red");
	}
	var mousemove = function(d) {
	  div1.html(d)
		.style("left", (d3.event.pageX)-30 + "px")
		.style("top", (d3.event.pageY -40) + "px");
	  //console.log(d);
	  div1.transition()
        .duration(200)
        .style("opacity", .9);
	}
	var mouseleave = function(d) {
	  div1.transition()
        .duration(500)
        .style("opacity", 0);
	  d3.select(this)
		.style("fill",function(d,i){return color[d];});
	}
	
	
	//사각형을 그리는 rect를 생성해서 svg에 추가하기
	svg.selectAll("rect")
	  .data(dict_data)
	  .enter()
	  .append("rect")
	  .attr("x", function(d, i){
		return offsetX;
	  })
	  .attr("y", function(d, i){
		return  i*((height-2.5*offsetY)/(xdata.length)) +1.8*offsetY;// svg 크기에 맞게 각각의 막대 자리를 나눠줌
	  })

	  .attr("width", function(d,i){return d*(width-(offsetX+110))/max;})
	  .attr("height",10)
	  .on("mouseover", mouseover)
	  .on("mousemove", mousemove)
	  .on("mouseleave", mouseleave)
	  .attr("fill",function(d,i){return color[i];});
	   
	var y = d3.scaleBand()
	  .domain(xdata)
	  .range([2.5*offsetY, height]);//축 길이?? 사이즈
	 
	   
	svg.append("g")
	  .attr("transform","translate("+offsetX+","+(-offsetY)+")")
	  .attr("class", "bary")
	  .call(d3.axisLeft(y))
	  .style("font-size","15px");
	  
	//legend
	//svg.append("rect").attr("x",650).attr("y",200).attr("width", 100).attr("height", 60).attr("stroke","black").attr("stroke-width","1").attr('fill', 'rgba(0,0,0,0)')
	svg.append("rect").attr("x",710).attr("y",205).attr("width", 18).attr("height", 18).style("fill", "rgb(212,122,254)")
	svg.append("rect").attr("x",710).attr("y",230).attr("width", 18).attr("height", 18).style("fill", "rgb(172,255,110)")
	svg.append("text").attr("x", 735).attr("y", 214).text("부정").style("font-size", "15px").attr("alignment-baseline","middle").attr("font-family", "'Jua', sans-serif")
	svg.append("text").attr("x", 735).attr("y", 239).text("긍정").style("font-size", "15px").attr("alignment-baseline","middle").attr("font-family", "'Jua', sans-serif")

  }