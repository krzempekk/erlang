defmodule PollutionData do
  @moduledoc false

  def importLinesFromCSV do
    File.read!("pollution.csv")
    |> String.split("\r\n")
  end

  def parseDate(date) do
    date
    |> String.split("-")
    |> Enum.reverse
    |> Enum.map(&String.to_integer/1)
    |> List.to_tuple
  end

  def parseTime(time) do
    time
    |> String.split(":")
    |> Enum.map(&String.to_integer/1)
    |> (& &1 ++ [0]).()
    |> List.to_tuple
  end

  def parseLocation(longitude, latitude) do
    { String.to_float(longitude), String.to_float(latitude) }
  end

  def parseValue(value) do
    String.to_integer(value)
  end

  def parseLine(line) do
    [date, time, longitude, latitude, pollutionLevel] = String.split(line, ",")
    %{}
    |> Map.put(:datetime, { parseDate(date), parseTime(time) })
    |> Map.put(:location, parseLocation(longitude, latitude))
    |> Map.put(:pollutionLevel, parseValue(pollutionLevel))
  end

  def parseLines(lines) do
    for line <- lines, do: parseLine(line)
  end

  def identifyStations(values) do
    Enum.uniq_by(values, fn value -> value[:location] end)
  end

  def formatName(location) do
    "station_#{Float.to_string(elem(location, 0))}_#{Float.to_string(elem(location, 1))}}"
  end

  def loadStationDataToServer(values) do
    values
    |> identifyStations
    |> Enum.map(fn (value) -> :pollution_gen_server.addStation(formatName(value[:location]), value[:location]) end)
    |> Enum.count(& &1 === :ok)
  end

  def loadValuesDataToServer(values) do
    values
    |> Enum.map(fn (value) -> :pollution_gen_server.addValue(value[:location], value[:datetime], "PM10", value[:pollutionLevel]) end)
    |> Enum.count(& &1 === :ok)
  end

  def measureTime(function) do
    function
    |> :timer.tc
    |> elem(0)
  end
end