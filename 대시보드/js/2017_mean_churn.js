// 그래프 크기 설정 
var margin = {top: 100, right: 50, bottom: 50, left: 100}
  , width = 500- margin.left - margin.right 
  , height = 550 - margin.top - margin.bottom; 

//데이터 불러오기
xas2=[]
yas2=[]
d3.json("./data./2017_mean_churn.json",function(error,data){
    data.forEach(function (d) {
        yas2.push({"y" : d.value});
        xas2.push(d.date);
    })

// 5. X scale will use the index of our data
var xScale1 = d3.scaleLinear()
    .domain([0, 12]) // input
    .range([0, width]); // output

// 6. Y scale will use the randomly generate number 
var yScale0 = d3.scaleLinear()
    .domain([2.0, 2.8]) // input 
    .range([height, 0]); // output 

// 7. d3's line generator
var line1 = d3.line()
    .x(function(d, i) { return xScale1(i+1); }) // set the x values for the line generator
    .y(function(d) { return yScale0(d.y); }) // set the y values for the line generator 

// 8. An array of objects of length N. Each object has key -> value pair, the key being "y" and the value is a random number
var dataset1 = yas2

// 1. Add the SVG to the page and employ #2
var svg1 = d3.select("#row4_2").append("svg")
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
    .append("g")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

// 3. Call the x axis in a group tag
svg1.append("g")
    .attr("class", "x axis")
    .attr("class", "barName") 
    .attr("transform", "translate(0," + (height+30) + ")")
    .call(d3.axisBottom(xScale1)); // Create an axis component with d3.axisBottom

// 4. Call the y axis in a group tag
svg1.append("g")
    .attr("class", "y axis")
    .attr("class", "bary")
    .call(d3.axisLeft(yScale0) // Create an axis component with d3.axisLeft
    .ticks(5));

// 9. Append the path, bind the data, and call the line generator 
svg1.append("path")
    .datum(dataset1) // 10. Binds data to the line 
    .attr("class", "line") // Assign a class for styling 
    .attr("d", line1); // 11. Calls the line generator 

// 12. Appends a circle for each datapoint 
var div1 = d3.select("body").append("div")
    .attr("class", "tooltip")
    .style("opacity", 0);

svg1.selectAll(".dot")
    .data(dataset1)
    .enter().append("circle") // Uses the enter().append() method
    .attr("class", "dot") // Assign a class for styling
    .attr("cx", function(d, i) { return xScale1(i+1) })
    .attr("cy", function(d) { return yScale0(d.y) })
    .attr("r", 5)
    .on("mouseover", function(d,i) {
        div1.transition()
          .duration(200)
          .style("opacity", .9)
        d3.select(this)
          .style("fill","red");
        div1.html((i+1)+"월"+"<br/>" +d.y )
          .style("left", (d3.event.pageX)-30 + "px")
          .style("top", (d3.event.pageY -40) + "px");

        d3.select(this)
        svg1.append('line')
        .transition()
        .duration(300)
        .attr('x1', 0)
        .attr('y1', yScale0(d.y))
        .attr('x2', 1090)
        .attr('y2', yScale0(d.y))
        .attr('stroke', 'red')
        .attr('stroke-dasharray','2')
    })	
      .on("mouseout", function(d) {
        div1.transition()
          .duration(500)
          .style("opacity", 0)
        d3.select(this)
          .style("fill","orange");
          d3.selectAll("line").style("display", "none")

        
        });
  });



// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  \
xas3=[]
yas3=[]
  d3.json("./data./bottom.json",function(error,data){
    data.forEach(function (d) {
        yas3.push({"y" : d.value});
        xas3.push(d.date);
    })
    var xScale1 = d3.scaleLinear()
    .domain([0, 12]) // input
    .range([0, width]); // output

// 6. Y scale will use the randomly generate number 
var yScale0 = d3.scaleLinear()
    .domain([0, 15.0]) // input 
    .range([height, 0]); // output 

// 7. d3's line generator
var line1 = d3.line()
    .x(function(d, i) { return xScale1(i); }) // set the x values for the line generator
    .y(function(d) { return yScale0(d.y-1.5); }) // set the y values for the line generator 
    .curve(d3.curveMonotoneX) // apply smoothing to the line
var line2 = d3.line()
    .x(function(d, i) { return xScale1(i); }) // set the x values for the line generator
    .y(function(d) { return yScale0(d.y-2.0); }) // set the y values for the line generator 
    .curve(d3.curveMonotoneX) // apply smoothing to the line

// 8. An array of objects of length N. Each object has key -> value pair, the key being "y" and the value is a random number
var dataset1 = yas3

// 1. Add the SVG to the page and employ #2
var svg1 = d3.select("#row4_2").append("svg")
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
    .append("g")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

svg1.append("path")
    .datum(dataset1) // 10. Binds data to the line 
    .attr("class", "bottomline") // Assign a class for styling 

// 9. Append the path, bind the data, and call the line generator 
svg1.append("path")
    .datum(dataset1) // 10. Binds data to the line 
    .attr("class", "bottomline") // Assign a class for styling 
    .attr("d", line1); // 11. Calls the line generator 
svg1.append("path")
    .datum(dataset1) // 10. Binds data to the line 
    .attr("class", "bottomline") // Assign a class for styling 
    .attr("d", line2); // 11. Calls the line generator 
  });