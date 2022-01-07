var margin = {top: 50, right: 50, bottom: 50, left: 100},
    width = 500 - margin.left - margin.right,
    height = 500 - margin.top - margin.bottom;

var x = d3.scaleLinear()
    .domain([0,2200])
    .range([0, width]);

var y = d3.scaleLinear()
    .domain([800,105000])
    .range([height, 0]);

var color = d3.scaleOrdinal(d3.schemeCategory10);

var xAxis = d3.axisBottom(x);

var yAxis = d3.axisLeft(y);

var svg = d3.select("#row4_1").append("svg")
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
    .append("g")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")");
//-------까지 동일

//1번
d3.json("./data./1.json", function(error, data) {
  if (error) throw error;

  point=[]
  value=[]
  churn=[]
  data.forEach(function(d,i) {
    point.push(d.points_in_wallet)
    value.push(d.avg_transaction_value)
    churn.push(d.churn_risk_score)
  });

  //x.domain(d3.extent(data, function(d) { return d.points_in_wallet; })).nice();
  //y.domain(d3.extent(data, function(d) { return d.avg_transaction_value; })).nice();

  svg.append("g")
      .attr("class", "x axis")
      .attr("class", "bary") 
      .attr("transform", "translate(0," + height + ")")
      .call(xAxis)
    .append("text")
      .attr("class", "label")
      .attr("x", width)
      .attr("y", -6)
      .style("text-anchor", "end")


  svg.append("g")
      .attr("class", "y axis")
      .attr("class", "bary")
      .call(yAxis)
    .append("text")
      .attr("class", "label")
      .attr("transform", "rotate(-90)")
      .attr("y", 6)
      .attr("dy", ".71em")
      .style("text-anchor", "end")

  svg.selectAll(".dot1")
      .data(data)
    .enter().append("circle")
      .attr("class", "dot1")
      .attr("r", 3)
      .attr("cx", function(d) { return x(d.points_in_wallet); })
      .attr("cy", function(d) { return y(d.avg_transaction_value); })
      .style("fill", function(d) { return color(d.churn_risk_score); });

  var legend = svg.selectAll(".legend")
      .data(color.domain())
    .enter().append("g")
      .attr("class", "legend")
      .attr("transform", function(d, i) { return "translate(0," + i * 20 + ")"; });

  legend.append("rect")
      .attr("x", width - 18)
      .attr("width", 18)
      .attr("height", 18)
      .style("fill", color)
      .on("mouseover",function(d){
        svg.selectAll(".dot2")
          .style("opacity","0")
        svg.selectAll(".dot3")
          .style("opacity","0")
      })
      .on("mouseout",function(d){
        svg.selectAll(".dot2")
          .style("opacity","1")
        svg.selectAll(".dot3")
          .style("opacity","1")
      });
// rgb(44, 160, 44) : 3
// rgb(255, 127, 14) :2
// rgb(31, 119, 180) :1

  legend.append("text")
      .attr("x", width + 12)
      .attr("y", 9)
      .attr("dy", ".35em")
      .style("text-anchor", "end")
      .text(function(d) { return d; });

});

//2번
d3.json("./data./2.json", function(error, data) {
  if (error) throw error;

  point=[]
  value=[]
  churn=[]
  data.forEach(function(d,i) {
    point.push(d.points_in_wallet)
    value.push(d.avg_transaction_value)
    churn.push(d.churn_risk_score)
  });

  //x.domain(d3.extent(data, function(d) { return d.points_in_wallet; })).nice();
  //y.domain(d3.extent(data, function(d) { return d.avg_transaction_value; })).nice();



  svg.selectAll(".dot2")
      .data(data)
    .enter().append("circle")
      .attr("class", "dot2")
      .attr("r", 3)
      .attr("cx", function(d) { return x(d.points_in_wallet); })
      .attr("cy", function(d) { return y(d.avg_transaction_value); })
      .style("fill", function(d) { return color(d.churn_risk_score); });

  var legend = svg.selectAll(".legend")
      .data(color.domain())
    .enter().append("g")
      .attr("class", "legend")
      .attr("transform", function(d, i) { return "translate(0," + i * 20 + ")"; });

  legend.append("rect")
      .attr("x", width - 18)
      .attr("width", 18)
      .attr("height", 18)
      .style("fill", color)
      .on("mouseover",function(d){
        svg.selectAll(".dot1")
          .style("opacity","0")
        svg.selectAll(".dot3")
          .style("opacity","0")
      })
      .on("mouseout",function(d){
        svg.selectAll(".dot1")
          .style("opacity","1")
        svg.selectAll(".dot3")
          .style("opacity","1")
      });
// rgb(44, 160, 44) : 3
// rgb(255, 127, 14) :2
// rgb(31, 119, 180) :1

  legend.append("text")
      .attr("x", width + 12)
      .attr("y", 9)
      .attr("dy", ".35em")
      .style("text-anchor", "end")
      .text(function(d) { return d; });

});

//3번
d3.json("./data./3.json", function(error, data) {
  if (error) throw error;

  point=[]
  value=[]
  churn=[]
  data.forEach(function(d,i) {
    point.push(d.points_in_wallet)
    value.push(d.avg_transaction_value)
    churn.push(d.churn_risk_score)
  });

  //x.domain(d3.extent(data, function(d) { return d.points_in_wallet; })).nice();
  //y.domain(d3.extent(data, function(d) { return d.avg_transaction_value; })).nice();


  



  svg.selectAll(".dot3")
      .data(data)
    .enter().append("circle")
      .attr("class", "dot3")
      .attr("r", 3)
      .attr("cx", function(d) { return x(d.points_in_wallet); })
      .attr("cy", function(d) { return y(d.avg_transaction_value); })
      .style("fill", function(d) { return color(d.churn_risk_score); });

  var legend = svg.selectAll(".legend")
      .data(color.domain())
    .enter().append("g")
      .attr("class", "legend")
      .attr("transform", function(d, i) { return "translate(0," + i * 20 + ")"; });

  legend.append("rect")
      .attr("x", width - 18)
      .attr("width", 18)
      .attr("height", 18)
      .style("fill", color)
      .on("mouseover",function(d){
        svg.selectAll(".dot2")
          .style("opacity","0")
        svg.selectAll(".dot1")
          .style("opacity","0")
      })
      .on("mouseout",function(d){
        svg.selectAll(".dot2")
          .style("opacity","1") 
        svg.selectAll(".dot1")
          .style("opacity","1")
      });
// rgb(44, 160, 44) : 3
// rgb(255, 127, 14) :2
// rgb(31, 119, 180) :1

  legend.append("text")
      .attr("x", width + 12)
      .attr("y", 9)
      .attr("dy", ".35em")
      .style("text-anchor", "end")
      .text(function(d) { return d; });

});