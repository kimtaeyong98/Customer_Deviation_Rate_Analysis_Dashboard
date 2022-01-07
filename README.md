# Customer_Deviation_Rate_Analysis_Dashboard
캐글에서 제공된 이탈률 분석 데이터를 기반으로 이탈률 원인을 분석한 결과를 기반으로  제작한 분석 대시보드 웹사이트 입니다.

다중로지스틱회귀, 배깅, 부스팅, 랜덤포레스트, one vs all , 의사결정나무, svm 를 이용했습니다.

종속변수는 이탈위험도 상 중 하 3가지로 분류하여 진행했습니다.

train 데이터와 test 데이터로 분류하여 진행했고, test 데이터에 대해 96퍼센트 정확도로 예측하는 의사결정나무를 모델로 채택하고 구성한 웹 대시보드 입니다.

데이터 : https://www.kaggle.com/imsparsh/churn-risk-rate-hackerearth-ml

# 전체 뷰

![image](https://user-images.githubusercontent.com/63800086/148571120-014b94b2-ac95-40c7-b3d8-e3a94979205e.png)


각 그래프나 요소에 마우스 오버시 효과가 나타납니다. / 멤버십별 필터링을 하여 데이터를 확인 할 수 있습니다.

ex)

![image](https://user-images.githubusercontent.com/63800086/148571545-34823abf-0320-44ed-9381-e15eee92fadf.png)



![image](https://user-images.githubusercontent.com/63800086/148571406-ebe4fd4d-d3a8-4613-a841-ab203cc78451.png)


