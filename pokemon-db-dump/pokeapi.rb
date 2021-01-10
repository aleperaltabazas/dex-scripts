require "rest-client"
require "json"

class Object
  def let(&f)
    f.call(self)
  end

  def take_if(&condition)
    if condition.call(self)
      self
    else
      nil
    end
  end
end

first = ARGV[0]
last = ARGV[1]

file = File.join(File.dirname(__FILE__), "pokemons.sql")

class String
  def escape
    "'#{self}'"
  end
end

for dex_number in first..last
  puts "Fetching ##{dex_number}"
  poke = RestClient.get("https://pokeapi.co/api/v2/pokemon/#{dex_number}").body.let do |b| JSON.parse(b) end
  species = RestClient.get("https://pokeapi.co/api/v2/pokemon-species/#{dex_number}").body.let do |b| JSON.parse(b) end

  name = poke["name"]
  primary_ability = poke["abilities"][0]["ability"]["name"]
  secondary_ability = poke["abilities"].find { |x| x["slot"] == 2 }&.let { |x| x["ability"]["name"] }

  hidden_ability = poke["abilities"].find { |a| a["is_hidden"] }&.let { |x| x["ability"]["name"] }
  primary_type = poke["types"].find { |t| t["slot"] == 1 }["type"]["name"]
  secondary_type = poke["types"].find { |t| t["slot"] == 2 }&.let { |x| x["type"]["name"] }
  female_probability = species["gender_rate"].take_if { |g| g != -1 }&.let { |x| (x * 100) / 8 }
  male_probability = female_probability&.let { |x| 100 - x }
  hp = poke["stats"][0]["base_stat"]
  attack = poke["stats"][1]["base_stat"]
  defense = poke["stats"][2]["base_stat"]
  sp_attack = poke["stats"][3]["base_stat"]
  sp_defense = poke["stats"][4]["base_stat"]
  speed = poke["stats"][5]["base_stat"]

  File.open(file, "a") do |f| f.puts %{
    insert into pokemons (name, national_dex_number, primary_ability, secondary_ability, hidden_ability, primary_type,
                          secondary_type, male_prob, female_prob, hp, attack, defense, special_attack, special_defense,
                          speed)
    values (
        '#{name}', 
        #{dex_number}, 
        '#{primary_ability}', 
        #{secondary_ability&.escape || "null"}, 
        #{hidden_ability&.escape || "null"}, 
        '#{primary_type.upcase}', 
        #{secondary_type&.upcase&.escape || "null"}, 
        #{male_probability || "null"}, 
        #{female_probability || "null"}, 
        #{hp}, 
        #{attack}, 
        #{defense}, 
        #{sp_attack}, 
        #{sp_defense}, 
        #{speed}
        );}   end
end
