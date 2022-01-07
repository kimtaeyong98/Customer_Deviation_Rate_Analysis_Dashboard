// 그래프 크기 설정 
var margin = {top: 50, right: 50, bottom: 50, left: 50}
  , width = 500- margin.left - margin.right 
  , height = 300 - margin.top - margin.bottom; 

//데이터 불러오기
xas=[]
yas=[]
d3.json("./data./2017_join2.json",function(error,data){
    data.forEach(function (d) {
        yas.push({"y" : d.value});
        xas.push(d.date);
    })

var xScale = d3.scaleLinear()
    .domain([0, 12]) 
    .range([0, width]); 
 
var yScale = d3.scaleLinear()
    .domain([500, 1000]) 
    .range([height, 0]); 


var line = d3.line()
    .x(function(d, i) { return xScale(i+1); }) 
    .y(function(d) { return yScale(d.y); }) 


var dataset = yas
console.log(dataset[0])


var svg = d3.select("#row4_3").append("svg")
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
    .append("g")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")");


svg.append("g")
    .attr("class", "x axis")
    .attr("class", "barName") 
    .attr("transform", "translate(0," + (height+30) + ")")
    .call(d3.axisBottom(xScale)); 
    
svg.append("g")
    .attr("class", "y axis")
    .attr("class", "bary")
    .call(d3.axisLeft(yScale)); 


svg.append("path")
    .datum(dataset) 
    .attr("class", "line")  
    .attr("d", line); 


var div = d3.select("body").append("div")
    .attr("class", "tooltip")
    .style("opacity", 0);

svg.selectAll(".dot")
    .data(dataset)
    .enter().append("circle") 
    .attr("class", "dot") 
    .attr("cx", function(d, i) { return xScale(i+1) })
    .attr("cy", function(d) { return yScale(d.y) })
    .attr("r", 5)
    .on("mouseover", function(d,i) {

        div.transition()
          .duration(200)
          .style("opacity", .9)
        d3.select(this)
          .style("fill","red");
        div.html((i+1)+"월"+"<br/>" +d.y )
          .style("left", (d3.event.pageX)-30 + "px")
          .style("top", (d3.event.pageY -40) + "px");

        d3.select(this)
        svg.append('line')
        .transition()
        .duration(300)
        .attr('x1', 0)
        .attr('y1', yScale(d.y))
        .attr('x2', 1090)
        .attr('y2', yScale(d.y))
        .attr('stroke', 'red')
        .attr('stroke-dasharray','2')
    })	
      .on("mouseout", function(d) {
        div.transition()
          .duration(500)
          .style("opacity", 0)
        d3.select(this)
          .style("fill","orange");
        d3.selectAll("line").style("display", "none")

        
        });
  });

  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
xas4=[]
yas4=[]
  d3.json("./data./bottom.json",function(error,data){
    data.forEach(function (d) {
        yas4.push({"y" : d.value});
        xas4.push(d.date);
    })
    var xScale1 = d3.scaleLinear()
    .domain([0, 12]) 
    .range([0, width]); 


var yScale0 = d3.scaleLinear()
    .domain([0, 15.0]) 
    .range([height, 0]); 


var line1 = d3.line()
    .x(function(d, i) { return xScale1(i); }) 
    .y(function(d) { return yScale0(d.y-1.5); })  
    .curve(d3.curveMonotoneX) 
var line2 = d3.line()
    .x(function(d, i) { return xScale1(i); }) 
    .y(function(d) { return yScale0(d.y-2.0); }) 
    .curve(d3.curveMonotoneX) 


var dataset1 = yas4


var svg1 = d3.select("#row4_3").append("svg")
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
    .append("g")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

svg1.append("path")
    .datum(dataset1) 
    .attr("class", "bottomline") 


svg1.append("path")
    .datum(dataset1) 
    .attr("class", "bottomline") 
    .attr("d", line1);  
svg1.append("path")
    .datum(dataset1) 
    .attr("class", "bottomline")  
    .attr("d", line2); 
  });  